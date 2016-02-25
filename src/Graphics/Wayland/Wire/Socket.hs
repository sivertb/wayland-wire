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
import Control.Exception
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
import Prelude
import System.Posix
import System.IO.Unsafe

data Socket = Socket S.Socket (MVar Raw)

withSocket :: Socket -> (S.Socket -> IO b) -> IO b
withSocket (Socket sock _) f = f sock

wrapSocket :: S.Socket -> IO Socket
wrapSocket s = Socket s <$> newMVar mempty

-- | Gets the full path of the socket.
-- This function replicates what wayland does in it's add_socket function.
socketAddr :: Maybe String -> IO S.SockAddr
socketAddr name = do
    prefix   <- getEnvDefault "XDG_RUNTIME_DIR" "/tmp"
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
socket :: IO Socket
socket = S.socket S.AF_UNIX S.Stream S.defaultProtocol >>= wrapSocket

-- | Creates and starts listening to a unix socket on the given path.
listen :: Maybe String  -- ^ The path to listen on.
       -> IO Socket      -- ^ The new socket.
listen name = do
    sock <- socket
    addr <- socketAddr name
    withSocket sock $ \s -> do
        S.bind s addr
        S.listen s 1
    return sock

-- | Creates and connects to a unix socket on the given path.
connect :: Maybe String -- ^ The path to connect to.
        -> IO Socket    -- ^ The new socket
connect name = do
    sock <- socket
    addr <- socketAddr name
    withSocket sock (`S.connect` addr)
    return sock

-- | Accepts an incoming connection on the socket.
-- The new socket inherits the lookup function from the listening socket. This
-- function will block until someone tries to connect.
accept :: Socket -> IO Socket
accept sock = withSocket sock S.accept >>= wrapSocket . fst

-- | Closes a socket. If this is a listening socket it will also remove the
-- socket file.
close :: Socket -> IO ()
close sock = withSocket sock $ \s -> do
    ls <- S.isListening s
    S.SockAddrUnix path <- S.getSocketName s
    S.close s
    when ls $ removeLink path

-- | Helper function for 'recv'.
--
-- Pulls data from the socket until it has enough to parse a full 'WireMsg'. If
-- the parse fails, it returns exception.
--
-- The function is meant to be called with masked exceptions. 'recvMsg' is
-- interruptible and can be interrupted even with exceptions masked, but
-- everything else is safe from asynchronous exceptions.
recvLoop :: MessageLookup -> S.Socket -> Raw -> IO (Raw, Either SomeException Message)
recvLoop lf sock oldInp = do
    res <- (Right <$> recvMsg sock 4096) `catch` (return . Left)
    case res of
      Left  err          -> return (oldInp, Left err)
      Right (msg, cmsgs) -> do
          let newInp = Raw msg (concat $ mapMaybe fdData cmsgs)
              inp    = oldInp <> newInp

          case runIncremental (getMsg lf) `pushInput` inp of
            Done left _ a -> return (left, Right a)
            Fail _    _ e -> return (inp , Left . SomeException $ userError e)
            _             -> recvLoop lf sock inp

-- | Receives a message from the socket.
recv :: MessageLookup -> Socket -> IO Message
recv lf (Socket sock mvar) = do
    res <- modifyMVarMasked mvar $ recvLoop lf sock

    case res of
      Left  err -> throwIO err
      Right msg -> return msg

-- | Sends a message on the socket.
send :: Socket -> Message -> IO ()
send (Socket sock _) msg = sendMsg sock bs [CMsg S.sOL_SOCKET S.sCM_RIGHTS (fdsToBs fds)]
    where
        (Raw bs fds) = runPut $ putMsg msg
