module Test.Fd
    ( fdExists
    , createFds
    , compareFds
    , closeFds
    )
where

import Control.Exception
import Control.Monad
import Data.Function
import Data.Maybe
import System.IO.Error
import System.Posix

fdExists :: Fd -> IO Bool
fdExists fd = catchIOError (getFdStatus fd >> return True) (\_ -> return False)

createFd :: Fd -> IO (Fd, FilePath)
createFd fd = do
    (path, handle) <- mkstemp "socket-test"
    newFd <- handleToFd handle
    unless (newFd == fd) $ do
        _ <- dupTo newFd fd
        closeFd newFd
    return (fd, path)

createFds :: [Fd] -> IO [(Fd, FilePath)]
createFds = mapM createFd

closeFds :: [(Fd, FilePath)] -> IO ()
closeFds = mapM_ (\(fd, path) -> closeFd fd >> removeLink path)

compareFds :: Fd -> Fd -> IO Bool
compareFds a b =
    handle (const $ return False :: SomeException -> IO Bool) $ do
        fsA <- getFdStatus a
        fsB <- getFdStatus b
        return $ on (==) deviceID fsA fsB && on (==) fileID fsA fsB
