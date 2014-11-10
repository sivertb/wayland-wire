module Graphics.Wayland.Wire.Socket
    ( Socket
    , recv
    , send
    )
where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Data.Binary.Put
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Decoder
import Graphics.Wayland.Wire.Message
import qualified Graphics.Wayland.Protocol as P
import qualified Network.Socket as S
import Network.Socket.Msg

data Socket = Socket (IO (ObjId -> OpCode -> Maybe [P.Argument])) (MVar (S.Socket, Decoder Message))

-- | Creates a 'Socket' from a normal socket and an IO computation
-- returning a mapping from 'ObjId' to '[P.Argument]'.
fromSocket :: S.Socket -> IO (ObjId -> OpCode -> Maybe [P.Argument]) -> IO Socket
fromSocket sock lf = Socket lf <$> newMVar (sock, return undefined)


-- | Checks if the 'CMsg' contains file descriptors, and if it does returns the
-- data.
fdData :: CMsg -> Maybe BS.ByteString
fdData cmsg = do
    guard (cmsgLevel cmsg == S.sOL_SOCKET && cmsgType cmsg == S.sCM_RIGHTS)
    return $ cmsgData cmsg

-- | Helper function for 'recv'. Pulls data from the socket until it has enough
-- to parse a full 'WireMsg'. If the parse fails, it throws an exception.
recvLoop :: S.Socket -> Decoder Message -> IO ((S.Socket, Decoder Message), Message)
recvLoop sock q = do
    (msg, _, cmsgs) <- recvMsg sock 4096
    let p = pushData msg (BS.concat $ mapMaybe fdData cmsgs) q
    case p of
         Done _ _ _ a -> return ((sock, p), a)
         Fail _ _ _ e -> ioError (userError e)
         _            -> recvLoop sock p

-- | Receives a message from the socket.
recv :: Socket -> IO Message
recv (Socket lf mvar) =
    modifyMVar mvar $ \(sock, q) -> lf >>= recvLoop sock . (q >>) . pullMsg

-- | Sends a message on the socket.
send :: Message -> Socket -> IO ()
send msg (Socket _ mvar) =
    withMVar mvar $ \(sock, _) -> do
        let dmsg = BSL.toStrict . runPut $ putMsg msg
            cmsg = BSL.toStrict . runPut $ putCmsg msg
        sendMsg sock dmsg Nothing [CMsg S.sOL_SOCKET S.sCM_RIGHTS cmsg]
