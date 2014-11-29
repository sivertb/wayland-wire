module Graphics.Wayland.Wire.Socket
    ( Socket
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
import System.IO.Unsafe

data Socket = Socket (IO MessageLookup) (MVar (S.Socket, Raw))

withSocket :: Socket -> (S.Socket -> IO b) -> IO b
withSocket (Socket _ mvar) f = withMVar mvar $ \(s, _) -> f s

wrapSocket :: IO MessageLookup -> S.Socket -> IO Socket
wrapSocket lf s = Socket lf <$> newMVar (s, mempty)

-- | Gets the full path of the socket.
-- This function replicates what wayland does in it's add_socket function.
socketAddr :: Maybe String -> IO S.SockAddr
socketAddr name = do
    prefix   <- getEnvDefault "XDG_RUNTIME_DIR" ""
    envName  <- getEnv "WAYLAND_DISPLAY"
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
socket :: IO MessageLookup -> IO Socket
socket lf = S.socket S.AF_UNIX S.Stream S.defaultProtocol >>= wrapSocket lf

-- | Creates and starts listening to a unix socket on the given path.
listen :: IO MessageLookup  -- ^ The lookup function to use when decoding messages.
       -> Maybe String      -- ^ The path to listen on.
       -> IO Socket         -- ^ The new socket.
listen lf name = do
    sock <- socket lf
    addr <- socketAddr name
    withSocket sock $ \s -> do
        S.bind s addr
        S.listen s 1
    return sock

-- | Creates and connects to a unix socket on the given path.
connect :: IO MessageLookup -- ^ The lookup function to use when decoding message.
        -> Maybe String     -- ^ The path to connect to.
        -> IO Socket        -- ^ The new socket
connect lf name = do
    sock <- socket lf
    addr <- socketAddr name
    withSocket sock $ \s -> S.connect s addr
    return sock

-- | Accepts an incoming connection on the socket.
-- The new socket inherits the lookup function from the listening socket. This
-- function will block until someone tries to connect.
accept :: Socket -> IO Socket
accept sock@(Socket lf _) = withSocket sock S.accept >>= wrapSocket lf . fst

-- | Closes a socket. If this is a listening socket it will also remove the
-- socket file.
close :: Socket -> IO ()
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
recv :: Socket -> IO Message
recv (Socket lf mvar) =
    modifyMVar mvar $ \(sock, inp) -> lf >>= recvLoop sock . flip pushInput inp . runIncremental . getMsg

-- | Sends a message on the socket.
send :: Socket -> Message -> IO ()
send (Socket _ mvar) msg =
    withMVar mvar $ \(sock, _) -> do
        let (Raw bs fds) = runPut $ putMsg msg
        sendMsg sock bs Nothing [CMsg S.sOL_SOCKET S.sCM_RIGHTS (fdsToBs fds)]
