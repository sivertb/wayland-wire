module Graphics.Wayland.Wire.Socket
    ( Socket
    , recv
    , send
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
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Get
import Graphics.Wayland.Wire.Put
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Raw
import qualified Graphics.Wayland.Protocol as P
import qualified Network.Socket as S
import Network.Socket.Msg
import System.Posix
import System.IO.Unsafe

data Socket = Socket (IO (ObjId -> OpCode -> Maybe [P.Argument])) (MVar (S.Socket, Raw))

-- | Creates a 'Socket' from a normal socket and an IO computation
-- returning a mapping from 'ObjId' to '[P.Argument]'.
fromSocket :: S.Socket -> IO (ObjId -> OpCode -> Maybe [P.Argument]) -> IO Socket
fromSocket sock lf = Socket lf <$> newMVar (sock, mempty)

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
send :: Message -> Socket -> IO ()
send msg (Socket _ mvar) =
    withMVar mvar $ \(sock, _) -> do
        let (Raw bs fds) = runPut $ putMsg msg
        sendMsg sock bs Nothing [CMsg S.sOL_SOCKET S.sCM_RIGHTS (fdsToBs fds)]
