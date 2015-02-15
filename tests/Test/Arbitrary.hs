module Test.Arbitrary
where

import Control.Applicative
import Data.Word
import qualified Data.ByteString as BS
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import Graphics.Wayland.Wire.Raw
import Graphics.Wayland.Protocol hiding (Argument)
import qualified Graphics.Wayland.Protocol as P
import System.Posix
import Test.QuickCheck

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

instance Arbitrary OpCode where
    arbitrary = OpCode <$> arbitrary
    shrink (OpCode o) = map OpCode (shrink o)

instance Arbitrary ObjId where
    arbitrary = ObjId <$> arbitrary
    shrink (ObjId o) = map ObjId (shrink o)

instance Arbitrary NewId where
    arbitrary = NewId <$> arbitrary
    shrink (NewId o) = map NewId (shrink o)

instance Arbitrary Message where
    arbitrary =
        Message
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink (Message a b c) = [ Message a' b' c' | (a', b', c') <- shrink (a, b, c) ]

instance Arbitrary Fd where
    arbitrary = fromIntegral <$> (choose (0, 1000) :: Gen Word32)
    shrink f = fromIntegral <$> shrink (fromIntegral f :: Word32)

instance Arbitrary Argument where
    arbitrary =
        oneof [ ArgInt    <$> arbitrary
              , ArgWord   <$> arbitrary
              , ArgFd     <$> arbitrary
              , ArgFixed . fromIntegral <$> (arbitrary :: Gen Word32)
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
