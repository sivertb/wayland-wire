{-# LANGUAGE TemplateHaskell #-}
module Test.Api.Gen
where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
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

prefix :: Bool -> String
prefix False = "C."
prefix True  = "S."

isNewType :: P.Type -> Bool
isNewType (P.TypeNew _ _) = True
isNewType _               = False

zipWithNew :: (Bool -> a) -> [P.Type] -> [a] -> [a]
zipWithNew _ []     _  = []
zipWithNew f (a:as) is =
    case (a, is) of
         (P.TypeNew n _, _   ) -> f n : zipWithNew f as is
         (_            , j:js) -> j   : zipWithNew f as js
         _                     -> error "This should never happen"

genReceiver :: Bool -> Name -> String -> (String, [P.Type]) -> Q Dec
genReceiver server r obj (func, args) = do
    objId      <- newName "o"
    var        <- newName "var"
    input      <- mapM (\_ -> newName "i") $ filter (not . isNewType) args
    Just cons  <- lookupValueName . (prefix server ++) . toCamelU $ printf "%s_%s" obj "Slots"
    Just field <- lookupValueName . (prefix server ++) . toCamelL $ printf "%s_%s" obj func
    funD r [ clause [varP objId] (normalB (body var input cons field objId)) [] ]
    where
        body var input cons field objId =
            [| do
                $(varP var) <- liftIO $ newEmptyMVar
                registerObject (Object $(varE objId)) $(recConE cons [ (,) field <$> recvFunc var input ] )
                recvAndDispatch
                liftIO $ readMVar $(varE var)
            |]
        recvFunc var input =
            lamE
                (zipWithNew (const wildP) args (map varP input))
                [| liftIO $ putMVar $(varE var) $(tupE (zipWith mkTupVar input (filter (not . isNewType) args))) |]
        mkTupVar i (P.TypeObject True  _) = [| fmap (Object . unObject) $(varE i) |]
        mkTupVar i (P.TypeObject False _) = [| Object . unObject $ $(varE i) |]
        mkTupVar i _                      = varE i

zipWithFixed :: (a -> a) -> [P.Type] -> [a] -> [a]
zipWithFixed _ []     _      = []
zipWithFixed f (a:as) (i:is) =
    case a of
         P.TypeFixed -> f i : zipWithFixed f as is
         _           -> i   : zipWithFixed f as is

genSender :: Bool -> Name -> String -> (String, [P.Type]) -> Q Dec
genSender server s obj (func, args) = do
    let side = if server
                 then ''Server
                 else ''Client

    objId        <- newName "o"
    input        <- mapM (\_ -> newName "i") $ filter (not . isNewType) args
    Just objType <- lookupTypeName  . (prefix server ++) $ toCamelU obj
    Just field   <- lookupValueName . (prefix server ++) . toCamelL $ printf "%s_%s" obj func
    signal       <- [| signals (Object $(varE objId) :: Object $(conT side) $(conT objType)) |]

    funD s [ clause (map varP (objId:input)) (normalB (return $ body signal field (zipWithNew newArg args (map VarE input)))) [] ]
    where
        body signal field as = foldl AppE (VarE field) (signal : zipWithFixed (AppE $ VarE 'fixedToDouble) args as)
        newArg True  = AppE (ConE 'Just) dummyCons
        newArg False = dummyCons
        dummyCons = LamE [WildP] $ AppE (VarE 'return) (VarE 'undefined)

genTest :: Bool -> String -> (String, [P.Type]) -> Q (Name, Dec)
genTest server obj func@(funcName, args) = do
    let testName = mkName $ printf "prop_%s_%s" obj funcName
    o     <- newName "o"
    input <- mapM (\_ -> newName "i") nonNewArgs
    out   <- mapM (\_ -> newName "x") nonNewArgs
    r     <- newName "reciever"
    s     <- newName "sender"
    (,) testName <$>
        funD testName
            [ clause (map varP (o:input)) (body r s o out input)
              [ genReceiver server r obj func
              , genSender (not server) s obj func
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
    <$> mapM (genTest True  (P.ifaceName iface)) (getRequests iface)
    <*> mapM (genTest False (P.ifaceName iface)) (getEvents   iface)

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
