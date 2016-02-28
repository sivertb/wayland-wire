{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Socket
where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.List
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Put
import Graphics.Wayland.Wire.Raw
import Graphics.Wayland.Wire.Socket
import qualified Network.Socket as S
import Network.Socket.Msg
import Prelude
import System.Posix
import Test.Fd
import Test.Message
import Test.QuickCheck
import Test.QuickCheck.Monadic

server :: (Socket -> IO a) -> MVar () -> MVar () -> IO a
server m fenceA fenceB =
    bracket (listen Nothing) close $ \ls -> do
    putMVar fenceA ()
    bracket (accept ls) close (\s -> m s <* takeMVar fenceB)

client :: (Socket -> IO a) -> MVar () -> MVar () -> IO a
client m fenceA fenceB = do
    liftIO $ takeMVar fenceA
    s <- connect Nothing
    a <- m s
    close s
    putMVar fenceB ()
    return a

runSockets :: (Socket -> IO a) -> (Socket -> IO b) -> IO (Maybe a, Maybe b)
runSockets a b = do
    ma     <- newEmptyMVar
    mb     <- newEmptyMVar
    fenceA <- newEmptyMVar
    fenceB <- newEmptyMVar
    _  <- forkFinally (server a fenceA fenceB) (finalizer ma)
    _  <- forkFinally (client b fenceA fenceB) (finalizer mb)
    ra <- takeMVar ma
    rb <- takeMVar mb
    return (ra, rb)
    where
        finalizer mvar (Left  e) = putStrLn ("exception in thread: " ++ show e) >> putMVar mvar Nothing
        finalizer mvar (Right x) = putMVar mvar (Just x)

msgFds :: Message -> [Fd]
msgFds = mapMaybe argToFd . msgArgs
    where
        argToFd (ArgFd f) = Just f
        argToFd _         = Nothing

cmpArg :: MsgArg -> MsgArg -> IO Bool
cmpArg (ArgFd fdA) (ArgFd fdB) = compareFds fdA fdB
cmpArg a           b           = return $ a == b

cmpMsg :: Message -> Message -> IO Bool
cmpMsg (Message opA objA argsA) (Message opB objB argsB) =
    ( and
    . ( [ opA  == opB
        , objA == objB
        , length argsA == length argsB
        ] ++)
    ) <$> zipWithM cmpArg argsA argsB

closeMsgFds :: Message -> IO ()
closeMsgFds = mapM_ closeFd . nub . msgFds

-- | Checks that none of the file descriptors exists, and that there's fewer
-- than 400.
--
-- The limit of 400 is to avoid us going above 1024 open file descriptors when
-- they are duplicated by sendMsg/recvMsg.
checkFds :: [Fd] -> PropertyM IO ()
checkFds fds = do
    anyFdsExists <- or <$> mapM (run . fdExists) fds
    pre (not anyFdsExists && length fds < 400)

-- | Verify that the message sent is equal to the one that's received.
prop_sendRecv :: Message -> Property
prop_sendRecv msg =
    monadicIO $ do
        checkFds fds
        (eq, res) <- run $ do
            paths <- createFds fds
            (_, res) <- runSockets (`send` msg) (recv lf)
            eq <- case res of
                       Nothing -> return False
                       Just m  -> cmpMsg msg m <* closeMsgFds m
            closeFds paths
            return (eq, res)
        stop (counterexample (show (Just msg) ++ " /= " ++ show res) eq)
    where
        lf  = testMsgLookup msg
        fds = nub $ msgFds msg

-- | Verify that its possible to receive two separate messages sent as a single
-- message.
prop_sendTwo :: Message -> Message -> Property
prop_sendTwo msgA msgB =
    monadicIO $ do
        checkFds fds

        (eq, res) <- run $ do
            paths  <- createFds fds

            (_, res) <-
                runSockets
                    (\(Socket s _) -> sendMsg s msgsBs [CMsg S.sOL_SOCKET S.sCM_RIGHTS (fdsToBs msgsFds)])
                    (\s -> (,) <$> recv lfA s <*> recv lfB s)

            eq <- case res of
                    Nothing       -> return False
                    Just (mA, mB) ->
                        (&&)
                        <$> cmpMsg msgA mA
                        <*> cmpMsg msgB mB
                        <*  closeMsgFds mA
                        <*  closeMsgFds mB

            closeFds paths

            return (eq, res)

        stop (counterexample (show (Just (msgA, msgB)) ++ " /= " ++ show res) eq)
    where
        (Raw msgsBs msgsFds) = runPut $ putMsg msgA >> putMsg msgB
        lfA = testMsgLookup msgA
        lfB = testMsgLookup msgB
        fds = nub $ msgFds msgA ++ msgFds msgB

-- | Verify that it's possible to send and receive multiple messages.
prop_sendSeveral :: Property
prop_sendSeveral = forAll genMsgs $ \msgs ->
    monadicIO $ do
        let fds = nub $ concatMap msgFds msgs
        checkFds fds
        (eq, res) <- run $ do
            paths  <- createFds fds

            (_, res) <-
                runSockets
                    (\s -> mapM_ (send s) msgs)
                    (\s -> mapM ((`recv` s) . testMsgLookup) msgs)

            eq <- case res of
                    Nothing -> return False
                    Just ms ->
                        and
                        <$> zipWithM cmpMsg ms msgs
                        <*  mapM_ closeMsgFds ms

            closeFds paths

            return (eq, res)

        stop (counterexample (show (Just msgs) ++ " /= " ++ show res) eq)
    where
        genMsgs = do
            len <- choose (0, 10)
            vectorOf len arbitrary

-- | Verify that it's possible to close a socket that's "running" accept.
prop_closeWhileAccept :: Property
prop_closeWhileAccept =
    monadicIO $ do
        run $ do
            ls  <- listen Nothing
            var <- newEmptyMVar
            _   <- forkFinally (accept ls) (putMVar var)
            yield
            close ls
            void $ readMVar var
        stop True

-- | Verify that it's possible to close a socket while it's using 'recv'.
prop_closeWhileRead :: Property
prop_closeWhileRead =
    monadicIO $ do
        run $ do
            ls  <- listen Nothing
            var <- newEmptyMVar
            _   <- forkFinally (void $ connect Nothing) (putMVar var)
            s   <- accept ls
            _   <- forkFinally (void $ recv undefined s) (putMVar var)
            close s

            void $ readMVar var
            void $ readMVar var
            close ls

        stop True

return []
socketTests :: IO Bool
socketTests = $quickCheckAll
