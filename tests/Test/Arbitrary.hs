module Test.Arbitrary
where

import Control.Applicative
import Data.Maybe
import Data.Word
import qualified Data.ByteString as BS
import Graphics.Wayland.Dispatch
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Raw
import Graphics.Wayland.Protocol
import Prelude
import System.Posix
import Test.QuickCheck hiding (Fixed)

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

instance Arbitrary Argument where
    arbitrary =
        Argument
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
        <*> arbitrary

instance Arbitrary Entry where
    arbitrary =
        Entry
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary OpCode where
    arbitrary = OpCode <$> arbitrary
    shrink (OpCode o) = map OpCode (shrink o)

instance Arbitrary ObjId where
    arbitrary = ObjId <$> (arbitrary `suchThat` (/= 0))
    shrink (ObjId o) = filter (/= 0) $ map ObjId (shrink o)

instance Arbitrary NewId where
    arbitrary = NewId <$> (arbitrary `suchThat` (/= 0))
    shrink (NewId o) = filter (/= 0) $ map NewId (shrink o)

instance Arbitrary Message where
    arbitrary =
        Message
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink (Message a b c) = [ Message a' b' c' | (a', b', c') <- shrink (a, b, c) ]

instance Arbitrary Fd where
    arbitrary = fromIntegral <$> (choose (50, 1000) :: Gen Word32)
    shrink f = fromIntegral <$> shrink (fromIntegral f :: Word32)

instance Arbitrary Fixed where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word32)
    shrink f  = fromIntegral <$> shrink (fromIntegral f :: Word32)

instance Arbitrary MsgArg where
    arbitrary =
        oneof [ ArgInt    <$> arbitrary
              , ArgWord   <$> arbitrary
              , ArgFd     <$> arbitrary
              , ArgFixed  <$> arbitrary
              , ArgString <$> arbitrary
              , ArgObject <$> suchThat arbitrary (/= Just 0)
              , ArgNew    <$> suchThat arbitrary (/= Just 0)
              , ArgArray  <$> arbitrary
              ]
    shrink arg =
        case arg of
             ArgInt    i -> ArgInt      <$> shrink i
             ArgWord   w -> ArgWord     <$> shrink w
             ArgFixed  f -> ArgFixed    <$> shrink f
             ArgFd     f -> ArgFd       <$> shrink f
             ArgString s -> ArgString   <$> shrink s
             ArgObject o -> ArgObject   <$> filter (/= Just 0) (shrink o)
             ArgNew    o -> ArgNew      <$> filter (/= Just 0) (shrink o)
             ArgArray  a -> ArgArray    <$> shrink a

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink = map BS.pack . shrink . BS.unpack

instance Arbitrary Raw where
    arbitrary = Raw <$> arbitrary <*> arbitrary

instance Arbitrary (Object c i) where
    arbitrary = Object <$> arbitrary

newtype ArbEnum e = ArbEnum { unArbEnum :: e }
    deriving (Eq, Show, Ord)

castArbEnum :: (WireEnum a, WireEnum b) => a -> ArbEnum b
castArbEnum = ArbEnum . fromJust . fromWord32 . toWord32

instance (Enum e, Bounded e, WireEnum e) => Arbitrary (ArbEnum e) where
    arbitrary = ArbEnum <$> arbitraryBoundedEnum
