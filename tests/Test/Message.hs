{-# LANGUAGE TemplateHaskell #-}
module Test.Message
    ( messageTests
    , msgLookup
    )
where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS
import Graphics.Wayland.Wire.Get
import Graphics.Wayland.Wire.Put
import Graphics.Wayland.Wire.Raw
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Protocol hiding (Argument)
import qualified Graphics.Wayland.Protocol as P
import Graphics.Wayland.Types
import Test.QuickCheck

instance (Eq a) => Eq (Decoder a) where
    (Done i o a) == (Done j p b) = i == j && o == p && a == b
    (Fail i o a) == (Fail j p b) = i == j && o == p && a == b
    (Partial _ ) == (Partial _ ) = True
    _            == _            = False

instance Arbitrary Interface where
    arbitrary =
        Interface Nothing
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Request where
    arbitrary =
        Request
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> return Nothing

instance Arbitrary Description where
    arbitrary = Description <$> arbitrary <*> arbitrary

instance Arbitrary P.Argument where
    arbitrary =
        P.Argument
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Type where
    arbitrary =
        oneof
            [ return TypeSigned
            , return TypeUnsigned
            , return TypeFixed
            , return TypeFd
            , return TypeArray
            , TypeString <$> arbitrary
            , TypeObject <$> arbitrary <*> arbitrary
            , TypeNew    <$> arbitrary <*> arbitrary
            ]

instance Arbitrary Event where
    arbitrary =
        Event
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Enum' where
    arbitrary =
        Enum'
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Entry where
    arbitrary =
        Entry
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Message where
    arbitrary =
        Message
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink (Message a b c) = [ Message a' b' c' | (a', b', c') <- shrink (a, b, c) ]

instance Arbitrary Argument where
    arbitrary =
        oneof [ ArgInt    <$> arbitrary
              , ArgWord   <$> arbitrary
              , ArgFixed . fromIntegral <$> (arbitrary :: Gen Word32)
              , ArgFd    . fromIntegral <$> (choose (0, 1000) :: Gen Word32)
              , ArgString <$> arbitrary
              , ArgObject <$> suchThat arbitrary (/= Just 0)
              , ArgNew    <$> suchThat arbitrary (/= Just 0)
              , ArgArray  <$> arbitrary
              ]
    shrink arg =
        case arg of
             ArgInt    i -> ArgInt      <$> shrink i
             ArgWord   w -> ArgWord     <$> shrink w
             ArgFixed  f -> ArgFixed . fromIntegral <$> shrink (fromIntegral f :: Word32)
             ArgFd     f -> ArgFd    . fromIntegral <$> shrink (fromIntegral f :: Word32)
             ArgString s -> ArgString   <$> shrink s
             ArgObject o -> ArgObject   <$> filter (/= Just 0) (shrink o)
             ArgNew    o -> ArgNew      <$> filter (/= Just 0) (shrink o)
             ArgArray  a -> ArgArray    <$> shrink a

getLookup :: Interface -> Bool -> (ObjId -> OpCode -> Maybe [P.Type])
getLookup iface evt =
    case evt of
         False -> \_ -> fmap (map argType . reqArgs)   . idx (ifaceRequests iface)
         True  -> \_ -> fmap (map argType . eventArgs) . idx (ifaceEvents   iface)
    where
        idx []     _ = Nothing
        idx (a:_ ) 0 = Just a
        idx (_:as) n = idx as (n - 1)

genArg :: P.Type -> Gen Argument
genArg t =
    case t of
         TypeSigned     -> ArgInt       <$> arbitrary
         TypeUnsigned   -> ArgWord      <$> arbitrary
         TypeFixed      -> ArgFixed . fromIntegral <$> (arbitrary :: Gen Word32)
         TypeFd         -> ArgFd    . fromIntegral <$> (arbitrary :: Gen Word32)
         TypeArray      -> ArgArray     <$> arbitrary
         TypeString n   -> ArgString    <$> marb n
         TypeObject n _ -> ArgObject    <$> suchThat (marb n) (/= Just 0)
         TypeNew    n _ -> ArgNew       <$> suchThat (marb n) (/= Just 0)
    where
        marb True  = arbitrary
        marb False = Just <$> arbitrary

genMessageFromInterface :: Interface -> Gen (Message, Bool)
genMessageFromInterface iface = do
    evt <- case (hasEvents iface, hasRequests iface) of
                (False, True ) -> return False
                (True,  False) -> return True
                (True,  True ) -> arbitrary
    let maxOps = case evt of
                      True  -> fromIntegral . length $ ifaceEvents   iface
                      False -> fromIntegral . length $ ifaceRequests iface
    op <- choose (0, maxOps - 1)
    let Just ts = getLookup iface evt 0 op
    msg <- Message op <$> arbitrary <*> mapM genArg ts
    return (msg, evt)

hasEvents :: Interface -> Bool
hasEvents = not . null . ifaceEvents

hasRequests :: Interface -> Bool
hasRequests = not . null . ifaceRequests

hasEventsOrRequests :: Interface -> Bool
hasEventsOrRequests iface = hasEvents iface || hasRequests iface

msgLookup :: Message -> (ObjId -> OpCode -> Maybe [P.Type])
msgLookup msg _ _ = Just $ map argToType (msgArgs msg)
    where
        argToType arg =
            case arg of
                 ArgInt    _ -> TypeSigned
                 ArgWord   _ -> TypeUnsigned
                 ArgFixed  _ -> TypeFixed
                 ArgFd     _ -> TypeFd
                 ArgString n -> TypeString (isNothing n)
                 ArgObject n -> TypeObject (isNothing n) (Just "")
                 ArgNew    n -> TypeNew    (isNothing n) (Just "") 
                 ArgArray  _ -> TypeArray
        
runPutGet :: Get Message -> Message -> Property
runPutGet get msg =
    let inp@(Raw bs _) = runPut $ putMsg msg
        off            = BS.length bs
    in (runIncremental get `pushInput` inp) === Done mempty off msg

-- | Test that Put followed by Get produces the input message.
prop_putGet :: Message -> Property
prop_putGet msg = runPutGet (getMsg $ msgLookup msg) msg

-- | Test that Put/Get works when the message is generated from a random interface.
prop_putGetInterface :: Interface -> Property
prop_putGetInterface iface =
    hasEventsOrRequests iface ==>
    forAll (genMessageFromInterface iface) $ \(msg, evt) ->
        runPutGet (getMsg $ getLookup iface evt) msg

return []
messageTests :: IO Bool
messageTests = $quickCheckAll
