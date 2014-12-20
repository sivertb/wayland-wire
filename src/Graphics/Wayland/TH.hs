{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Wayland.TH
where

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.Int
import Data.Word
import Graphics.Wayland.Dispatch
import Graphics.Wayland.Protocol hiding (Type)
import Graphics.Wayland.Wire
import qualified Graphics.Wayland.Protocol as P
import Language.Haskell.TH
import System.Posix (Fd)
import Text.XML.HXT.Core hiding (mkName)

data Side = Server | Client deriving (Eq, Show)
data RecordType = Slots | Signals deriving (Eq, Show)

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ []     = []
mapFirst f (a:as) = f a : as

sideName :: Side -> Name
sideName Client = ''Client
sideName Server = ''Server

rtName :: RecordType -> Name
rtName Slots   = ''Slots
rtName Signals = ''Signals

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

getSlots :: Side -> Interface -> [(String, [P.Type])]
getSlots Server = getRequests
getSlots Client = getEvents

getSignals :: Side -> Interface -> [(String, [P.Type])]
getSignals Server = getEvents
getSignals Client = getRequests

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
genObjectType s n i = first (wrapMaybe n . AppT obj) <$> genInterfaceType i
    where
        obj = AppT (ConT ''Object) (ConT $ sideName s)

-- | Generates the type of a new object.
--
-- If this is a Slot (incoming call), the new type will be
-- '(Object c i -> m (Slots c i m)) -> m (Object c i)'
-- The user supplies a function that given an object, returns a set of
-- functions handling calls to that object, and the new object is returned.
--
-- If this is a Signal (outgoing call), the new type will be
-- 'Object c i -> m (Slots c i m)'
-- That is a function that given an object returns a set of functions handling
-- calls to that object. In addition the new object is in the return type.
genNewType :: RecordType -> Side -> Bool -> Maybe String -> Q (Type, [Name], [Type])
genNewType rt s n iface = do
    (it, ns) <- genInterfaceType iface

    let c    = conT $ sideName s
        i    = return it
        m    = varT $ mkName "m"
        obj  = [t| Object $c $i |]
        cons = [t| $obj -> $m (Slots $c $i $m) |]

    case rt of
         Slots   -> (, ns, []) . wrapMaybe n <$> [t| $cons -> $m $obj |]
         Signals -> (, ns,   )
                    <$> (wrapMaybe n <$> cons)
                    <*> ((:[]) . wrapMaybe n <$> obj)

-- | Generates the type of a single function argument, returning both the type
-- and any type variables it might contain.
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

-- | Generates a function type.
genFuncType :: RecordType -> Side -> [P.Type] -> Q Type
genFuncType rt s ts = do
    (ts', is', rs') <- unzip3 <$> mapM (genArgType rt s) ts

    let is = concat is'
        rs = concat rs'
        t  = foldr (\a b -> AppT (AppT ArrowT a) b) (AppT (VarT $ mkName "m") (mkTuple rs)) ts'

    if null is
      then return t
      else forallT (map PlainTV is) (cxt [classP ''DispatchInterface (map varT is)]) (return t)

fieldName :: Interface -> String -> Name
fieldName iface n = mkNameL $ ifaceName iface ++ "_" ++ n

genRecord :: RecordType -> Side -> Interface -> Q [Dec]
genRecord rt s iface = do
    con <- recC conName (map mkField args)
    return [ DataInstD [] (rtName rt) types [con] [] ]
    where
        conName     = mkNameU $ ifaceName iface ++ prefix
        typeName    = mkNameU $ ifaceName iface
        m = VarT $ mkName "m"
        types = [ConT $ sideName s, ConT typeName, m]
        mkField (n, t) = (fieldName iface n, NotStrict, ) <$> genFuncType rt s t
        prefix = case rt of
                      Slots   -> "_slots"
                      Signals -> "_signals"
        args = case rt of
                    Slots   -> getSlots s iface
                    Signals -> getSignals s iface

genSingleDispatch :: Interface -> Name -> (Integer, (String, [P.Type])) -> MatchQ
genSingleDispatch iface slots (i, (n, ts)) =
    match
        (litP $ IntegerL i)
        (normalB [| undefined |])
        []

genDispatch :: Side -> Interface -> Q Exp
genDispatch s iface = do
    slots <- newName "slots"
    msg   <- newName "msg"
    op    <- newName "op"
    lamE [varP slots, varP msg] $
        caseE [| msgOp $(varE msg) |]
        ( map (genSingleDispatch iface slots) (zip [0..] $ getSlots s iface) ++
        [ match (varP op) (normalB [| fail ("Unknown opcode " ++ show $(varE op)) |]) [] ]
        )

genSignals :: Side -> Interface -> Q Exp
genSignals s iface = [| undefined |]

genDispatchInstance :: Side -> Interface -> Q [Dec]
genDispatchInstance s iface =
    [d|
        instance Dispatch $i $c where
            dispatch = $(genDispatch s iface)
            signals  = $(genSignals  s iface)
    |]
    where
        c = conT $ sideName s
        i = conT . mkNameU $ ifaceName iface


-- | Generates all the code for an interface.
generateInterface :: Side -> Interface -> Q [Dec]
generateInterface s iface =
    concat <$> sequence
    [ genEmptyType . mkName . toCamelU $ ifaceName iface
    , genDispatchInterfaceInst iface
    , genRecord Slots s iface
    , genRecord Signals s iface
    , genDispatchInstance s iface
    ]

-- | Generates code for a protocol.
generateProtocol :: Side -> Protocol -> Q [Dec]
generateProtocol s = fmap concat . mapM (generateInterface s) . protoInterfaces

-- | Generates code for a protocol specified in the given XML file.
generateFromXml :: Side -> FilePath -> Q [Dec]
generateFromXml side file = do
    proto <- runIO . runX $ xunpickleDocument xpickle [withRemoveWS yes] file
    case proto of
         [p] -> generateProtocol side p
         _   -> fail $ "Could not unpickle protocol file " ++ file
