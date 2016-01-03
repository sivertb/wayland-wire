{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Api.Gen
    ( runTest
    , genTests
    , testResult
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Writer
import Data.Either
import Data.Maybe
import Data.List
import qualified Graphics.Wayland.Protocol as P
import Graphics.Wayland.TH
import Graphics.Wayland.Dispatch
import Graphics.Wayland.W
import Graphics.Wayland.Wire
import qualified Test.Api.Client as C
import Language.Haskell.TH
import Prelude
import Test.Arbitrary ()
import Test.QuickCheck
import Text.Printf

type FT b = Free (TestF b)

data TestF b a =
    TestMessage Message a
  | TestResult b a
  deriving (Functor)

$(makeFree ''TestF)

instance MonadSend (Free (TestF b)) where
    sendMessage = testMessage

runTestF :: FT b a -> (a, [b], [Message])
runTestF free =
    let (a, w) = runWriter (foldFree f free)
    in (a, lefts w, rights w)
    where
        f (TestMessage m a) = tell [Right m] >> return a
        f (TestResult  b a) = tell [Left  b] >> return a

-- | Runs a test using two W computations.
runTest :: (AllocLimits ma, AllocLimits mb, Eq b, Show b)
        => ObjectManager ma (FT b)
        -> ObjectManager mb (FT b)
        -> (Message -> W ma (FT b) x)
        -> W mb (FT b) y
        -> (ObjectManager ma (FT b), ObjectManager mb (FT b), x, y, [b], [b])
runTest mgrA mgrB server client =
    let ((Right rc, mgrB'), resClient, [msg]) = runTestF $ runW client mgrB
        ((Right rs, mgrA'), resServer, []   ) = runTestF $ runW (server msg) mgrA
    in (mgrA', mgrB', rs, rc, resServer, resClient)

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
        m    = mkName "m"
        c    = ConT $ sideName side
    in ForallT [PlainTV m] [AppT (ConT ''Monad) (VarT m)] $ wrapMaybe n $ foldl AppT cons [c, obj, VarT m]

-- | Generates the receiving part of the test.
--
-- This is a function that will register an 'Object' and add a handler for a
-- single slot, then wait for a message from the sender.
genReceiver :: Side -> Name -> String -> (String, [P.Type]) -> Q Dec
genReceiver side r obj (func, args) = do
    objId      <- newName "o"
    input      <- mapM (\_ -> newName "i") $ filter (not . isNewType) args
    Just cons  <- lookupValueName . (prefix side ++) . toCamelU $ printf "%s_%s" obj "Slots"
    Just field <- lookupValueName . (prefix side ++) . toCamelL $ printf "%s_%s" obj func

    let body =
            [|
                \msg -> do
                    registerObject (Object $(varE objId)) $(recConE cons [(,) field <$> recvFunc] )
                    dispatchMessage msg
            |]
        recvFunc =
            lamE
                (zipWithNew (\_ _  -> wildP) args (map varP input))
                [| lift (testResult $(tupE (zipWith mkTupVar input (filter (not . isNewType) args)))) |]

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

        dummyCons = AppE (VarE 'const) (AppE (VarE 'return) (VarE 'undefined))

        bodyArgs = zipWithNew newArg args $ map VarE input
        body     = foldl AppE (VarE field) (signal : zipWithFixed (AppE $ VarE 'fixedToDouble) args bodyArgs)

    funD s [ clause (map varP (objId:input)) (normalB (return body)) [] ]

-- | Generates a test for a single slot.
genTest :: Side -> String -> (String, [P.Type]) -> Q (Name, Dec)
genTest side obj func@(funcName, args) = do
    let testName = mkName $ printf "prop_%s_%s" obj funcName
    o     <- newName "o"
    out   <- mapM (\_ -> newName "x") nonNewArgs
    input <- mapM (\_ -> newName "i") nonNewArgs
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
            [|
                let (_, _, _, _, [ $(tupP $ map varP out) ], []) =
                        runTest
                            newObjectManager
                            newObjectManager
                            ($(varE r) $(varE o))
                            $(return $ foldl AppE (VarE s) (map VarE (o:input)))
                in $(tupE (zipWithFixed (appE $ varE 'doubleToFixed) nonNewArgs (map varE out))) === $(tupE $ map varE input)
            |]

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
