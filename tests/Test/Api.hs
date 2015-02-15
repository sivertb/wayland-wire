{-# LANGUAGE TemplateHaskell #-}
module Test.Api
    ( apiTests )
where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Word
import Graphics.Wayland.Dispatch
import Graphics.Wayland.Types
import Graphics.Wayland.W
import Graphics.Wayland.Wire
import System.IO.Error
import qualified Test.Api.Client as C
import qualified Test.Api.Server as S
import Test.Arbitrary ()
import Test.QuickCheck
import Test.QuickCheck.Monadic

type PIO = PropertyM IO

runAndCatch :: IO a -> PIO a
runAndCatch m = do
    res <- run $ tryIOError m
    case res of
         Left  e -> fail $ "Caught IO error: " ++ show e
         Right a -> return a

getSockets :: Maybe String -> IO (Socket, Socket)
getSockets addr = do
    ls   <- listen addr
    cVar <- newEmptyMVar
    sVar <- newEmptyMVar

    _ <- forkFinally (accept ls) (putMVar sVar)
    _ <- forkFinally (connect addr) (putMVar cVar)

    -- This will throw an exception if it does not pattern match, but that's okay.
    Right cSock <- readMVar cVar
    Right sSock <- readMVar sVar

    close ls
    return (sSock, cSock)

runTest :: W Server IO a -> W Client IO b -> PIO (a, b)
runTest server client = do
    (ss, cs) <- runAndCatch $ getSockets Nothing
    cVar     <- run newEmptyMVar
    sVar     <- run newEmptyMVar

    _ <- runAndCatch $ forkFinally (runW ss server) (putMVar sVar . collapse)
    _ <- runAndCatch $ forkFinally (runW cs client) (putMVar cVar . collapse)

    a <- runAndCatch (readMVar sVar)
    b <- runAndCatch (readMVar cVar)

    runAndCatch $ close ss
    runAndCatch $ close cs

    case (a, b) of
         (Left e  , _       ) -> fail $ "Server error: " ++ e
         (_       , Left e  ) -> fail $ "Client error: " ++ e
         (Right a', Right b') -> return (a', b')
    where
        collapse (Left e)          = Left $ show e
        collapse (Right (Left e))  = Left $ show e
        collapse (Right (Right a)) = Right a

prop_return :: Property
prop_return = monadicIO $ runTest (return ()) (return ())

serverCall :: ObjId -> Word32 -> W Server IO ()
serverCall o w = S.wlDisplayDeleteId (signals (Object o :: Object Server S.WlDisplay)) w

clientCall :: ObjId -> W Client IO Word32
clientCall o = do
    var <- liftIO $ newEmptyMVar
    registerObject (Object o) (C.WlDisplaySlots { C.wlDisplayDeleteId = \x -> liftIO (putMVar var x) })
    recvAndDispatch
    liftIO $ readMVar var

prop_call :: ObjId -> Word32 -> Property
prop_call o w = monadicIO $ do
    pre (o /= 0)
    (_, w') <- runTest (serverCall o w) (clientCall o)
    stop (w === w')

return []
apiTests :: IO Bool
apiTests = $quickCheckAll
