{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Socket
where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.List
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Socket
import System.IO.Error
import System.Posix
import Test.Message
import Test.QuickCheck
import Test.QuickCheck.Monadic

newtype TestM a = TestM (ReaderT MessageLookup IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader MessageLookup
             , MonadIO
             )

instance SocketError TestM where
    sockErr = fail . show

instance SocketLookup TestM where
    msgLookup = ask

runTestM :: MessageLookup -> TestM a -> IO a
runTestM lf (TestM tm) = runReaderT tm lf

server :: MVar () -> Message -> TestM ()
server fence msg = do
    ls <- listen Nothing
    liftIO $ putMVar fence ()
    s  <- accept ls
    send s msg
    close s
    close ls

client :: MVar () -> TestM Message
client fence = do
    liftIO $ takeMVar fence
    s <- connect Nothing
    m <- recv s
    close s
    return m

runSockets :: IO a -> IO b -> IO (Maybe a, Maybe b)
runSockets a b = do
    ma <- newEmptyMVar
    mb <- newEmptyMVar
    _  <- forkFinally a (finalizer ma)
    _  <- forkFinally b (finalizer mb)
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

fdExists :: Fd -> IO Bool
fdExists fd = catchIOError (getFdStatus fd >> return True) (\_ -> return False)

createFd :: Fd -> IO FilePath
createFd fd = do
    (path, handle) <- mkstemp "socket-test"
    newFd <- handleToFd handle
    unless (newFd == fd) $ do
        _ <- dupTo newFd fd
        closeFd newFd
    return path

createFds :: [Fd] -> IO [FilePath]
createFds = mapM createFd

closeFds :: [Fd] -> IO ()
closeFds = mapM_ closeFd

compareFds :: Fd -> Fd -> IO Bool
compareFds a b = do
    let newLen = (fromIntegral a * fromIntegral b) `mod` 3323
    setFdSize a newLen
    fs <- getFdStatus b
    return $ newLen == fileSize fs

cmpArg :: Argument -> Argument -> IO Bool
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

-- | Verify that the message sent is equal to the one that's received.
prop_sendRecv :: Message -> Property
prop_sendRecv msg =
    monadicIO $ do
        pre =<< (not . or) <$> mapM (run . fdExists) (msgFds msg)
        (eq, res) <- run $ do
            let lf  = testMsgLookup msg
                fds = msgFds msg
            paths <- createFds $ nub fds
            fence <- newEmptyMVar
            (_, res) <- runSockets (runTestM lf $ server fence msg) (runTestM lf $ client fence)
            eq <- case res of
                       Nothing -> return False
                       Just m  -> cmpMsg msg m
            closeFds $ nub fds
            mapM_ removeLink paths
            return (eq, res)
        stop (counterexample (show (Just msg) ++ " /= " ++ show res) eq)

return []
socketTests :: IO Bool
socketTests = $quickCheckAll
