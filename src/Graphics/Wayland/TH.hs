{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Wayland.TH
    ( Side (..)
    , generateFromXml
    )
where

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Int
import Data.Word
import Graphics.Wayland.Dispatch
import Graphics.Wayland.Protocol hiding (Type)
import Graphics.Wayland.Types
import Graphics.Wayland.Wire
import qualified Graphics.Wayland.Protocol as P
import Language.Haskell.TH
import System.Posix (Fd)
import Text.XML.HXT.Core hiding (mkName)

data RecordType = Slot | Signal deriving (Eq, Show)
data Side = Server | Client deriving (Eq, Show)

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ []     = []
mapFirst f (a:as) = f a : as

sideName :: Side -> Name
sideName Client = ''Client
sideName Server = ''Server

-- | Converst a snake_case string to CamelCase, leaving the first character as
-- upper-case.
toCamelL :: String -> String
toCamelL []       = []
toCamelL ('_':ss) = toCamelU ss
toCamelL (s  :ss) = s : toCamelL ss

-- | Converst a snake_case string to CamelCase, leaving the first character as
-- lower-case.
toCamelU :: String -> String
toCamelU = mapFirst toUpper . toCamelL

-- | Creates a 'Name' beginning with lower case from a snake_case string.
mkNameL :: String -> Name
mkNameL = mkName . toCamelL

-- | Creates a 'Name' beginning with upper case from a snake_case string.
mkNameU :: String -> Name
mkNameU = mkName . toCamelU

-- | Generates an empty type declaration with the given name.
genEmptyType :: Name -> Q [Dec]
genEmptyType name = return [ DataD [] name [] [] [] ]

-- | Generates a class instance for 'DispatchInterface' for the given 'Interface'.
genDispatchInterfaceInst :: Interface -> Q [Dec]
genDispatchInterfaceInst iface =
    (: [])
    <$> instanceD
            (cxt [])
            [t| DispatchInterface $(conT . mkNameU $ ifaceName iface) |]
            [ funD 'interfaceName    $ funC (stringE $ ifaceName    iface)
            , funD 'interfaceVersion $ funC (intE    $ ifaceVersion iface)
            ]
    where
        funC e = [ clause [ wildP ] (normalB e) [] ]
        intE = litE . integerL . fromIntegral

getRequests :: Interface -> [(String, [P.Type])]
getRequests = map (reqName &&& (map argType . reqArgs)) . ifaceRequests

getEvents :: Interface -> [(String, [P.Type])]
getEvents = map (eventName &&& (map argType . eventArgs)) . ifaceEvents

-- | Wraps a type in 'Maybe' if the first argument is 'True'.
wrapMaybe :: Bool -> Type -> Type
wrapMaybe True c = AppT (ConT ''Maybe) c
wrapMaybe _    c = c

-- | Generates the type of an interface, depending on its name. If it is
-- 'Nothing', an unique type variable will be generated, otherwise the type
-- corresponding to the interface is returned.
genInterfaceType :: Maybe String -> Q (Type, [Name])
genInterfaceType = maybe ((VarT &&& (: [])) <$> newName "i") (return . (, []) . ConT . mkNameU)

-- | Generates the type of an object.
genObjectType :: Side -> Bool -> Maybe String -> Q (Type, [Name])
genObjectType s n i =
    first (wrapMaybe n)
    <$> maybe
            (return (ConT ''ObjId, []))
            (return . (, []) . AppT obj . ConT . mkNameU)
            i
    where
        obj = AppT (ConT ''Object) (ConT $ sideName s)

-- | Generates the type of a new object.
--
-- If this is a Slot (incoming call), the new type will be
--
-- 'forall i . DispatchInterface => SlotConstructor c i m'
--
-- for calls where the interface is not know at compile time and
--
-- 'SlotConstructor c Interface m'
--
-- where it is known at compile time. 'c' is 'Server' or 'Client'.
--
-- If this is a Signal (outgoing call), the new type will be
--
-- 'SignalConstructor c i m'
--
-- and 'i' is either a type variable or a specific interface. The new object is
-- also added to the return type of the function.
genNewType :: RecordType -> Side -> Bool -> Maybe String -> Q (Type, [Name], [Type])
genNewType rt s nullable iface = do
    (it, ns) <- genInterfaceType iface

    let c    = conT $ sideName s
        i    = return it
        m    = varT $ mkName "m"
        obj  = [t| Object $c $i |]

    case rt of
         Slot ->
             (, [], [])
             <$> forallT
                 (map PlainTV ns)
                 (cxt $ map classCxt ns)
                 [t| SlotConstructor $c $i $m |]
         Signal ->
             (, ns, )
             <$> (wrapMaybe nullable <$> [t| SignalConstructor $c $i $m |])
             <*> ((: []) . wrapMaybe nullable <$> obj)

-- | Generates the type of a single function argument, returning both the type,
-- any type variables it contains that needs too be caught and optionally a
-- list of types to add to the function's return value.
genArgType :: RecordType -> Side -> P.Type -> Q (Type, [Name], [Type])
genArgType rt s t =
    case t of
         TypeSigned     -> (, [], []) <$> [t| Int32     |]
         TypeUnsigned   -> (, [], []) <$> [t| Word32    |]
         TypeFixed      -> (, [], []) <$> [t| Double    |]
         TypeFd         -> (, [], []) <$> [t| Fd        |]
         TypeArray      -> (, [], []) <$> [t| [Word32]  |]
         TypeString n   -> return (wrapMaybe n $ ConT ''String, [], [])
         TypeObject n i -> (\(a,b) -> (a, b, [])) <$> genObjectType s n i
         TypeNew    n i -> genNewType rt s n i

-- | Creates a tuple with the given list of element types.
-- The list can be empty, in which case it will create the '()' type.
-- If the list contains a single type, this type is returned.
mkTuple :: [Type] -> Type
mkTuple ts = foldl AppT (TupleT $ length ts) ts

-- | Returns a class predicate for 'DispatchInterface' for the given name.
classCxt :: Name -> PredQ
classCxt i = classP ''DispatchInterface [varT i]

-- | Generates a function type.
genFuncType :: RecordType -> Side -> [P.Type] -> Q Type
genFuncType rt s ts = do
    (ts', is', rs') <- unzip3 <$> mapM (genArgType rt s) ts

    let is  = concat is'
        rs  = concat rs'
        ret = AppT (VarT $ mkName "m") (mkTuple rs)
        t   = foldr (\a b -> AppT (AppT ArrowT a) b) ret ts'

    forallT
        (map PlainTV is)
        (cxt $ map classCxt is)
        (return t)

fieldName :: Interface -> String -> Name
fieldName iface prefix = mkNameL $ ifaceName iface ++ "_" ++ prefix

genRecord :: RecordType -> Side -> String -> Name -> [(String, [P.Type])] -> Interface -> Q Dec
genRecord rt s prefix recName args iface =
    dataInstD (pure []) recName types [con] []
    where
        con            = recC conName $ map mkField args
        conName        = mkNameU $ ifaceName iface ++ prefix
        typeName       = mkNameU $ ifaceName iface
        types          = [ conT $ sideName s
                         , conT $ typeName
                         , varT $ mkName "m"
                         ]
        mkField (n, t) = (fieldName iface n, NotStrict, ) <$> genFuncType rt s t

getSlots :: Side -> Interface -> [(String, [P.Type])]
getSlots Server = getRequests
getSlots Client = getEvents

getSignals :: Side -> Interface -> [(String, [P.Type])]
getSignals Server = getEvents
getSignals Client = getRequests

genSlotsRec :: Side -> Interface -> Q Dec
genSlotsRec s iface =
    genRecord Slot s "_slots" ''Slots (getSlots s iface) iface

genSignalsRec :: Side -> Interface -> Q Dec
genSignalsRec s iface =
    genRecord Signal s "_signals" ''Signals (getSignals s iface) iface

genDispatchArg :: P.Type -> Q ([Name], ExpQ)
genDispatchArg t =
    case t of
         TypeNew    nullable interface -> do
             name <- newName "name"
             ver  <- newName "ver"
             new  <- newName "new"

             let (ns, just) = case interface of
                                   Just _  -> ([new           ], [| Nothing                          |])
                                   Nothing -> ([name, ver, new], [| Just ($(varE name), $(varE ver)) |])
                 arg  = if nullable
                          then [| fmap (regObject $just) $(varE new) |]
                          else [| regObject $just $(varE new) |]

             return (ns, arg)

         TypeObject nullable (Just _) -> do
             obj <- newName "obj"
             let arg = if nullable
                         then [| fmap Object $(varE obj) |]
                         else [| Object $(varE obj) |]
             return ([obj], arg)

         _ -> ((: []) &&& varE) <$> newName "arg"

genSingleDispatch :: Interface -> (Integer, (String, [P.Type])) -> MatchQ
genSingleDispatch iface (i, (n, ts)) = do
    (pats, args') <- first concat . unzip <$> mapM genDispatchArg ts
    args          <- sequence args'

    let func  = lamE (map varP pats) (return $ foldl AppE field args)
        field = AppE (VarE $ fieldName iface n) (VarE $ mkName "slots")

    match
        (litP $ IntegerL i)
        (normalB [| fromMessage $(varE $ mkName "msg") $(func) |])
        []

genDispatch :: Side -> Interface -> Q Exp
genDispatch s iface = do
    lamE [varP $ mkName "slots", varP $ mkName "msg"] $
        caseE [| msgOp $(varE $ mkName "msg") |]
        ( map (genSingleDispatch iface ) (zip [0..] (getSlots s iface)) ++
        [ match (varP op) (normalB [| fail ("Unknown opcode " ++ show $(varE op)) |]) [] ]
        )
    where
        op = mkName "op"

genSignals :: Side -> Interface -> Q Exp
genSignals s iface = [| undefined |]

genDispatchInstance :: Side -> Interface -> Q [Dec]
genDispatchInstance s iface =
    (: [])
    <$> instanceD (cxt []) [t| Dispatch $i $c |]
        [ genSlotsRec   s iface
        , genSignalsRec s iface
        , funD 'dispatch [clause [] (normalB $ genDispatch s iface) []]
        , funD 'signals  [clause [] (normalB $ genSignals  s iface) []]
        ]
    where
        c = conT $ sideName s
        i = conT . mkNameU $ ifaceName iface

-- | Generates all the code for an interface.
generateInterface :: Side -> Interface -> Q [Dec]
generateInterface s iface =
    concat <$> sequence
    [ genEmptyType . mkName . toCamelU $ ifaceName iface
    , genDispatchInterfaceInst iface
    , genDispatchInstance s iface
    ]

-- | Generates code for a protocol.
generateProtocol :: Side -> Protocol -> Q [Dec]
generateProtocol s = fmap concat . mapM (generateInterface s) . protoInterfaces

-- | Generates code for a protocol specified in the given XML file.
generateFromXml :: Side -> FilePath -> Q [Dec]
generateFromXml s file = do
    proto <- runIO . runX $ xunpickleDocument xpickle [withRemoveWS yes] file
    case proto of
         [p] -> generateProtocol s p
         _   -> fail $ "Could not unpickle protocol file " ++ file
