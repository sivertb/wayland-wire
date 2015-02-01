module Graphics.Wayland.Wire.Socket
    ( Socket
    , SocketClass (..)
    -- * Sending and receiving data
    , recv
    , send
    -- * Socket operations
    , connect
    , listen
    , close
    , accept
    )
where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign
import Graphics.Wayland.Wire.Get
import Graphics.Wayland.Wire.Put
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Raw
import qualified Network.Socket as S
import Network.Socket.Msg
import System.Posix
import System.IO.Error
import System.IO.Unsafe

newtype Socket = Socket (MVar (S.Socket, Raw))

class (Functor m, MonadIO m) => SocketClass m where
    msgLookup :: m MessageLookup
    sockErr   :: IOError -> m a

-- | Lifts an IO computation to the 'W' monad, and catches any IO exceptions.
catchIO :: SocketClass m => IO a -> m a
catchIO m = do
    res <- liftIO $ (Right <$> m) `catchIOError` (return . Left)
    case res of
         Right a -> return a
         Left  e -> sockErr e

withSocket :: SocketClass m => Socket -> (S.Socket -> IO b) -> m b
withSocket (Socket mvar) f = catchIO . withMVar mvar $ \(s, _) -> f s

wrapSocket :: SocketClass m => S.Socket -> m Socket
wrapSocket s = Socket <$> liftIO (newMVar (s, mempty))

-- | Gets the full path of the socket.
-- This function replicates what wayland does in it's add_socket function.
socketAddr :: SocketClass m => Maybe String -> m S.SockAddr
socketAddr name = do
    prefix   <- liftIO $ getEnvDefault "XDG_RUNTIME_DIR" "/tmp"
    envName  <- liftIO $ getEnv "WAYLAND_DISPLAY"
    let sockName = case (name, envName) of
                        (Just s, _     ) -> s
                        (_     , Just s) -> s
                        (_     , _     ) -> "wayland-0"
    return . S.SockAddrUnix $ prefix ++ "/" ++ sockName

-- | Converts a 'ByteString' to a list of file descriptors.
bsToFds :: BS.ByteString -> [Fd]
bsToFds bs =
    unsafePerformIO
    . BS.unsafeUseAsCStringLen bs
    $ \(ptr, len) -> peekArray (len `div` 4) (castPtr ptr)

-- | Converts a list of file descriptors to a 'ByteString'.
fdsToBs :: [Fd] -> BS.ByteString
fdsToBs fds =
    unsafePerformIO
    . withArrayLen fds
    $ \len ptr -> BS.packCStringLen (castPtr ptr, len * 4)

-- | Checks if the 'CMsg' contains file descriptors, and if it does returns the
-- data.
fdData :: CMsg -> Maybe [Fd]
fdData cmsg = do
    guard (cmsgLevel cmsg == S.sOL_SOCKET && cmsgType cmsg == S.sCM_RIGHTS)
    return . bsToFds $ cmsgData cmsg

-- | Creates a new unix socket.
socket :: SocketClass m => m Socket
socket = catchIO (S.socket S.AF_UNIX S.Stream S.defaultProtocol) >>= wrapSocket

-- | Creates and starts listening to a unix socket on the given path.
listen :: SocketClass m
       => Maybe String  -- ^ The path to listen on.
       -> m Socket      -- ^ The new socket.
listen name = do
    sock <- socket
    addr <- socketAddr name
    withSocket sock $ \s -> do
        S.bind s addr
        S.listen s 1
    return sock

-- | Creates and connects to a unix socket on the given path.
connect :: SocketClass m
        => Maybe String -- ^ The path to connect to.
        -> m Socket    -- ^ The new socket
connect name = do
    sock <- socket
    addr <- socketAddr name
    withSocket sock $ \s -> S.connect s addr
    return sock

-- | Accepts an incoming connection on the socket.
-- The new socket inherits the lookup function from the listening socket. This
-- function will block until someone tries to connect.
accept :: SocketClass m => Socket -> m Socket
accept sock = withSocket sock S.accept >>= wrapSocket . fst

-- | Closes a socket. If this is a listening socket it will also remove the
-- socket file.
close :: SocketClass m => Socket -> m ()
close sock = withSocket sock $ \s -> do
    ls <- S.isListening s
    S.SockAddrUnix path <- S.getSocketName s
    S.close s
    when ls $ removeLink path

-- | Helper function for 'recv'. Pulls data from the socket until it has enough
-- to parse a full 'WireMsg'. If the parse fails, it throws an exception.
recvLoop :: S.Socket -> Decoder Message -> IO ((S.Socket, Raw), Message)
recvLoop sock q = do
    (msg, _, cmsgs) <- recvMsg sock 4096
    let p = pushInput q (Raw msg (concat $ mapMaybe fdData cmsgs))
    case p of
         Done i _ a -> return ((sock, i), a)
         Fail _ _ e -> ioError (userError e)
         _          -> recvLoop sock p

-- | Receives a message from the socket.
recv :: SocketClass m => Socket -> m Message
recv (Socket mvar) = do
    lf <- msgLookup
    catchIO . modifyMVar mvar $ \(sock, inp) -> recvLoop sock $ pushInput (runIncremental $ getMsg lf) inp

-- | Sends a message on the socket.
send :: SocketClass m => Socket -> Message -> m ()
send (Socket mvar) msg =
    catchIO . withMVar mvar $ \(sock, _) -> do
        let (Raw bs fds) = runPut $ putMsg msg
        sendMsg sock bs Nothing [CMsg S.sOL_SOCKET S.sCM_RIGHTS (fdsToBs fds)]
