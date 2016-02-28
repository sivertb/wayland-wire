module Test.Fd
where

import Control.Exception
import Control.Monad
import System.IO.Error
import System.Posix

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
compareFds a b =
    handle (const $ return False :: SomeException -> IO Bool) $ do
        setFdSize a newLen
        fs <- getFdStatus b
        return $ newLen == fileSize fs
    where
        newLen = (fromIntegral a * fromIntegral b) `mod` 3323
