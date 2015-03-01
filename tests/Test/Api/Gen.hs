{-# LANGUAGE TemplateHaskell #-}
module Test.Api.Gen
    ( runTest
    , genTests
    )
where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import qualified Graphics.Wayland.Protocol as P
import Graphics.Wayland.TH
import Graphics.Wayland.Dispatch
import Graphics.Wayland.W
import Graphics.Wayland.Wire
import Graphics.Wayland.Wire.Message
import System.IO.Error
import qualified Test.Api.Client as C
import Language.Haskell.TH
import System.Posix
import Test.Arbitrary ()
import Test.Fd
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Text.Printf

type PIO = PropertyM IO

-- | Run an IO computation in the PropertyM monad, and catch any exceptions.
runAndCatch :: IO a -> PIO a
runAndCatch m = do
    res <- run $ tryIOError m
    case res of
         Left  e -> fail $ "Caught IO error: " ++ show e
         Right a -> return a

-- | Creates two connected sockets.
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

-- | Runs a test using two W computations.
--
-- The W computations will be given a socket from a pair of connected sockets.
runTest :: (AllocLimits ma, AllocLimits mb) => W ma IO a -> W mb IO b -> PIO (a, b)
runTest server client = do
    (ss, cs) <- runAndCatch $ getSockets Nothing
    cVar     <- run newEmptyMVar
    sVar     <- run newEmptyMVar

    _ <- runAndCatch . forkFinally (runW ss server) $ (\e -> printEx e >> putMVar sVar e) . collapse
    _ <- runAndCatch . forkFinally (runW cs client) $ (\e -> printEx e >> putMVar cVar e) . collapse

    a <- runAndCatch (readMVar sVar)
    b <- runAndCatch (readMVar cVar)

    runAndCatch $ close ss
    runAndCatch $ close cs

    case (a, b) of
         (Left e  , _       ) -> fail $ "Server error: " ++ e
         (_       , Left e  ) -> fail $ "Client error: " ++ e
         (Right a', Right b') -> return (a', b')
    where
        printEx (Left e) = putStrLn e
        printEx _        = return ()

        collapse (Left e)          = Left $ show e
        collapse (Right (Left e))  = Left $ show e
        collapse (Right (Right a)) = Right a

protocol :: P.Protocol
protocol = C.testProtocol

-- | Returns the prefix to use for API names.
prefix :: Side -> String
prefix Client = "C."
prefix Server = "S."

-- | Returns the opposite side.
otherSide :: Side -> Side
otherSide Client = Server
otherSide Server = Client

-- | Checks if this is a new type.
isNewType :: P.Type -> Bool
isNewType (P.TypeNew _ _) = True
isNewType _               = False

-- | Apply a function to all new types.
zipWithNew :: (Bool -> Maybe String -> a) -> [P.Type] -> [a] -> [a]
zipWithNew _ []     _  = []
zipWithNew f (a:as) is =
    case (a, is) of
         (P.TypeNew n i, _   ) -> f n i : zipWithNew f as is
         (_            , j:js) -> j     : zipWithNew f as js
         _                     -> error "This should never happen"

-- | Returns the name of an object with the specified interface, using the
-- first interface if the interface isn't specified.
objectName :: Side -> Maybe String -> Name
objectName side = mkName . (prefix side ++) . toCamelU . fromMaybe (P.ifaceName . head $ P.protoInterfaces protocol)

-- | Creates a 'SignalConstructor' type.
signalConsType :: Side -> Bool -> Maybe String -> Type
signalConsType side n i =
    let cons = ConT ''SignalConstructor
        obj  = ConT (objectName side i)
        w    = AppT (AppT (ConT ''W) c) (ConT ''IO)
        c    = ConT $ sideName side
    in wrapMaybe n $ foldl AppT cons [c, obj, w]

-- | Generates the receiving part of the test.
--
-- This is a function that will register an 'Object' and add a handler for a
-- single slot, then wait for a message from the sender.
genReceiver :: Side -> Name -> String -> (String, [P.Type]) -> Q Dec
genReceiver side r obj (func, args) = do
    objId      <- newName "o"
    var        <- newName "var"
    input      <- mapM (\_ -> newName "i") $ filter (not . isNewType) args
    Just cons  <- lookupValueName . (prefix side ++) . toCamelU $ printf "%s_%s" obj "Slots"
    Just field <- lookupValueName . (prefix side ++) . toCamelL $ printf "%s_%s" obj func

    let body =
            [| do
                $(varP var) <- liftIO $ newEmptyMVar
                registerObject (Object $(varE objId)) $(recConE cons [(,) field <$> recvFunc] )
                recvAndDispatch
                liftIO $ readMVar $(varE var)
            |]
        recvFunc =
            lamE
                (zipWithNew (\_ _  -> wildP) args (map varP input))
                [| liftIO $ putMVar $(varE var) $(tupE (zipWith mkTupVar input (filter (not . isNewType) args))) |]

        mkTupVar i (P.TypeObject True  _) = [| fmap (Object . unObject) $(varE i) |]
        mkTupVar i (P.TypeObject False _) = [| Object . unObject $ $(varE i) |]
        mkTupVar i _                      = varE i

    funD r [ clause [varP objId] (normalB body) [] ]

-- | Apply a function to all fixed arguments.
zipWithFixed :: (a -> a) -> [P.Type] -> [a] -> [a]
zipWithFixed _ []     _      = []
zipWithFixed f (a:as) (i:is) =
    case a of
         P.TypeFixed -> f i : zipWithFixed f as is
         _           -> i   : zipWithFixed f as is

-- | Generates the sender part of the test.
--
-- It will generate a function that calls a single slot on an 'Object'.
genSender :: Side -> Name -> String -> (String, [P.Type]) -> Q Dec
genSender side s obj (func, args) = do
    objId        <- newName "o"
    input        <- mapM (\_ -> newName "i") $ filter (not . isNewType) args
    Just objType <- lookupTypeName  . (prefix side ++) $ toCamelU obj
    Just field   <- lookupValueName . (prefix side ++) . toCamelL $ printf "%s_%s" obj func
    signal       <- [| signals (Object $(varE objId) :: Object $(conT $ sideName side) $(conT objType)) |]

    let newArg True  i = newArgSig True  i $ AppE (ConE 'Just) dummyCons
        newArg False i = newArgSig False i $ dummyCons

        newArgSig n i v = SigE v $ signalConsType side n i

        dummyCons = LamE [WildP] $ AppE (VarE 'return) (VarE 'undefined)

        bodyArgs = zipWithNew newArg args $ map VarE input
        body     = foldl AppE (VarE field) (signal : zipWithFixed (AppE $ VarE 'fixedToDouble) args bodyArgs)

    funD s [ clause (map varP (objId:input)) (normalB (return body)) [] ]

-- | Generates a test for a single slot.
genTest :: Side -> String -> (String, [P.Type]) -> Q (Name, Dec)
genTest side obj func@(funcName, args) = do
    let testName = mkName $ printf "prop_%s_%s" obj funcName
    o     <- newName "o"
    input <- mapM (\_ -> newName "i") nonNewArgs
    out   <- mapM (\_ -> newName "x") nonNewArgs
    r     <- newName "reciever"
    s     <- newName "sender"
    (,) testName <$>
        funD testName
            [ clause (map varP (o:input)) (body r s o out input)
              [ genReceiver side r obj func
              , genSender (otherSide side) s obj func
              ]
            ]
    where
        nonNewArgs = filter (not . isNewType) args
        body r s o out input =
            normalB
            [| monadicIO $ do
                let fds       = $(listE $ findFds input nonNewArgs)
                    expNonFds = $(tupE  $ findNonFds (map varE input) nonNewArgs)
                pre ($(varE o) /= 0)
                pre =<< (not . or) <$> mapM (run . fdExists) fds
                paths <- run $ createFds fds
                ($(tupP $ map varP out), _) <-
                    runTest
                        ($(varE r) $(varE o))
                        $(return $ foldl AppE (VarE s) (map VarE (o:input)))
                let actNonFds = $(tupE  $ findNonFds (zipWithFixed (appE $ varE 'doubleToFixed) nonNewArgs (map varE out)) nonNewArgs)
                    actFds    = $(listE $ findFds out nonNewArgs)
                assert =<< and <$> run (zipWithM compareFds fds actFds)
                run . closeFds $ nub fds
                mapM_ (run . removeLink) paths
                stop $ actNonFds === expNonFds
            |]

        findNonFds []     _      = []
        findNonFds (i:is) (a:as) =
            case a of
                 P.TypeFd -> findNonFds is as
                 _        -> i : findNonFds is as

        findFds []     _      = []
        findFds (i:is) (a:as) =
            case a of
                 P.TypeFd -> varE i : findFds is as
                 _        -> findFds is as

-- | Generates tests for all requests and events of an interface
genInterfaceTests :: P.Interface -> Q [(Name, Dec)]
genInterfaceTests iface =
    (++)
    <$> mapM (genTest Server (P.ifaceName iface)) (getRequests iface)
    <*> mapM (genTest Client (P.ifaceName iface)) (getEvents   iface)

-- | Generates test cases for all requests and events in the protocol.
genTests :: Name -> Q [Dec]
genTests check = do
    (tests, decs) <- unzip . concat <$> mapM genInterfaceTests (P.protoInterfaces protocol)
    (: decs) <$>
        funD
            (mkName "allTests")
            [ clause
              []
              ( normalB
                ( listE
                $ map (\n -> [| ($(stringE $ show n), $(varE check) $(varE n)) |]) tests)
              )
              []
            ]
