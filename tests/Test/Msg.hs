{-# LANGUAGE TemplateHaskell #-}
module Test.Msg
    ( msgTests
    )
where

import Control.Exception
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.Msg
import System.IO.Error
import System.Posix.Files
import Test.Arbitrary ()
import Test.QuickCheck
import Test.QuickCheck.Monadic

withSockets :: ((Socket, Socket) -> IO a) -> IO a
withSockets =
    bracket
        mkSockets
        (\(a, b) -> close a >> close b >> removeLink path)
    where
        path = "test-socket"
        addr = SockAddrUnix path
        mkSockets = do
            sockL <- socket AF_UNIX Stream defaultProtocol
            sockA <- socket AF_UNIX Stream defaultProtocol
            bind sockL addr
            listen sockL 1
            connect sockA addr
            (sockB, _) <- accept sockL
            close sockL
            return  (sockA, sockB)

prop_sendRecv :: BS.ByteString -> Property
prop_sendRecv bs =
    BS.length bs <= bufSize && not (BS.null bs) ==> monadicIO $ do
        (bs', cmsgs') <- run . withSockets $ \(sockA, sockB) -> do
            sendMsg sockA bs []
            recvMsg sockB bufSize
        stop (bs' === bs .&&. [] === cmsgs')
    where
        bufSize = 4096

-- | Check that calling 'recvMsg' on a closed socket throws an exception.
prop_eof :: BS.ByteString -> Property
prop_eof bs =
    BS.length bs <= bufSize && not (BS.null bs) ==> monadicIO $ do
        ((bs', cmsgs'), err) <- run . withSockets $ \(sockA, sockB) -> do
            sendMsg sockA bs []
            msg <- recvMsg sockB bufSize
            close sockA
            err <- catch (Right <$> recvMsg sockB bufSize) (return . Left)
            return (msg, err)
        stop (bs' === bs
             .&&. [] === cmsgs'
             .&&. counterexample (show err) (either isEOFError (const False) err))
    where
        bufSize = 4096


return []
msgTests :: IO Bool
msgTests = $quickCheckAll
