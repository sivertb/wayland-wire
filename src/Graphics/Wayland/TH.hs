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

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst _ []     = []
mapFirst f (a:as) = f a : as

sideName :: Side -> Name
sideName Client = 'Client
sideName Server = 'Server

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
genObjectType :: Bool -> Maybe String -> Q (Type, [Name])
genObjectType n i = first (wrapMaybe n . AppT obj) <$> genInterfaceType i
    where
        obj = AppT (ConT ''Object) (VarT $ mkName "c")

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
genNewType :: Bool -> Maybe String -> Q (Type, [Name], [Type])
genNewType n iface = do
    (it, ns) <- genInterfaceType iface

    let c    = varT $ mkName "c"
        i    = return it
        m    = varT $ mkName "m"
        r    = varT $ mkName "r"
        cons = [t| Constructor $r $c $i $m |]
        obj  = [t| Object $c $i |]

    (, ns, )
        <$> (wrapMaybe n <$> cons)
        <*> ((: []) . wrapMaybe n <$> obj)

-- | Generates the type of a single function argument, returning both the type
-- and any type variables it might contain.
genArgType :: P.Type -> Q (Type, [Name], [Type])
genArgType t =
    case t of
         TypeSigned     -> (, [], []) <$> [t| Int32     |]
         TypeUnsigned   -> (, [], []) <$> [t| Word32    |]
         TypeFixed      -> (, [], []) <$> [t| Double    |]
         TypeFd         -> (, [], []) <$> [t| Fd        |]
         TypeArray      -> (, [], []) <$> [t| [Word32]  |]
         TypeString n   -> return (wrapMaybe n $ ConT ''String, [], [])
         TypeObject n i -> (\(a,b) -> (a, b, [])) <$> genObjectType n i
         TypeNew    n i -> genNewType n i

-- | Creates a tuple with the given list of element types.
-- The list can be empty, in which case it will create the '()' type.
-- If the list contains a single type, this type is returned.
mkTuple :: [Type] -> Type
mkTuple ts = foldl AppT (TupleT $ length ts) ts

-- | Returns a class predicate for 'DispatchInterface' for the given name.
classCxt :: Name -> PredQ
classCxt i = classP ''DispatchInterface [varT i]

-- | Return an equality predicate checking that
-- 'UnCons (Constructor r c i m) ~ P r c i m'.
-- This is used as a proof that 'Constructor' is injective.
equalCxt :: Name -> PredQ
equalCxt i =
    let r = varT $ mkName "r"
        c = varT $ mkName "c"
        m = varT $ mkName "m"
    in  equalP
            [t| UnCons (Constructor $r $c $(varT i) $m) |]
            [t| P $r $c $(varT i) $m |]

-- | Generates a function type.
genFuncType :: [P.Type] -> Q Type
genFuncType ts = do
    (ts', is', rs') <- unzip3 <$> mapM genArgType ts

    let is  = concat is'
        rs  = concat rs'
        ret = AppT
                (VarT $ mkName "m")
                (AppT
                    (AppT (ConT ''RetType) (VarT $ mkName "r"))
                    (mkTuple rs)
                )
        t   = foldr (\a b -> AppT (AppT ArrowT a) b) ret ts'

    if null is
      then return t
      else forallT
                (map PlainTV is)
                (cxt $ map classCxt is ++ map equalCxt is)
                (return t)

fieldName :: Interface -> String -> Name
fieldName iface prefix = mkNameL $ ifaceName iface ++ "_" ++ prefix

genRecord :: String -> Name -> [(String, [P.Type])] -> Interface -> Q [Dec]
genRecord prefix recName args iface =
    (: []) <$> dataInstD (pure []) recName types [con] []
    where
        con            = recC conName $ map mkField args
        conName        = mkNameU $ ifaceName iface ++ prefix
        typeName       = mkNameU $ ifaceName iface
        types          = [ varT $ mkName "r"
                         , varT $ mkName "c"
                         , conT $ typeName
                         , varT $ mkName "m"
                         ]
        mkField (n, t) = (fieldName iface n, NotStrict, ) <$> genFuncType t

genRequests :: Interface -> Q [Dec]
genRequests iface = genRecord "_requests" ''Requests (getRequests iface) iface

genEvents :: Interface -> Q [Dec]
genEvents iface = genRecord "_events" ''Events (getEvents iface) iface

genSingleDispatch :: Interface -> Name -> (Integer, (String, [P.Type])) -> MatchQ
genSingleDispatch iface slots (i, (n, ts)) =
    match
        (litP $ IntegerL i)
        (normalB [| undefined |])
        []

{-
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

genDispatchInstance :: Interface -> Q [Dec]
genDispatchInstance iface =
    [d|
        instance Dispatch $i $c where
            dispatch = $(genDispatch iface)
            signals  = $(genSignals  iface)
    |]
    where
        c = conT $ sideName s
        i = conT . mkNameU $ ifaceName iface
-}

-- | Generates all the code for an interface.
generateInterface :: Interface -> Q [Dec]
generateInterface iface =
    concat <$> sequence
    [ genEmptyType . mkName . toCamelU $ ifaceName iface
    -- , genDispatchInterfaceInst iface
    , genRequests iface
    , genEvents iface
    -- , genDispatchInstance s iface
    ]

-- | Generates code for a protocol.
generateProtocol :: Protocol -> Q [Dec]
generateProtocol = fmap concat . mapM generateInterface . protoInterfaces

-- | Generates code for a protocol specified in the given XML file.
generateFromXml :: FilePath -> Q [Dec]
generateFromXml file = do
    proto <- runIO . runX $ xunpickleDocument xpickle [withRemoveWS yes] file
    case proto of
         [p] -> generateProtocol p
         _   -> fail $ "Could not unpickle protocol file " ++ file
