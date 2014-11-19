{-# LANGUAGE TemplateHaskell #-}
module Test.Get
    ( getTests )
where

import Control.Applicative
import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS
import Foreign
import Graphics.Wayland.Wire.Get
import Graphics.Wayland.Wire.Raw
import System.Posix
import Test.QuickCheck

data TestGet =
    TestGetData Int Int
  | TestGetOffset
  | TestGetFail String
  deriving (Show, Eq)

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink = map BS.pack . shrink . BS.unpack

instance Arbitrary Fd where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word32)

instance Arbitrary Raw where
    arbitrary = Raw <$> arbitrary <*> arbitrary

instance Arbitrary TestGet where
    arbitrary =
        oneof [ TestGetData <$> suchThat arbitrary (> 0) <*> suchThat arbitrary (> 0)
              , TestGetFail <$> arbitrary
              , return TestGetOffset
              ]

instance (Eq a) => Eq (Decoder a) where
    (Done i o a) == (Done j p b) = i == j && o == p && a == b
    (Fail i o a) == (Fail j p b) = i == j && o == p && a == b
    (Partial _ ) == (Partial _ ) = True
    _            == _            = False

makeGet :: TestGet -> Get (BS.ByteString, [Fd], [Offset])
makeGet (TestGetData i j) = (\(Raw bs fs) -> (bs, fs, [])) <$> getData i j
makeGet (TestGetOffset  ) = (\o -> (BS.empty, [], [o])) <$> getOffset
makeGet (TestGetFail e  ) = getFail e

-- | Generates a parser from a list of 'TestGet' values.
makeGets :: [TestGet] -> Get (BS.ByteString, [Fd], [Offset])
makeGets = fmap mconcat . mapM makeGet

-- | Generates the expected output after running all the parser in the list on
-- the given input.
expectedDecoder :: [TestGet] -> Raw -> Decoder (BS.ByteString, [Fd], [Offset])
expectedDecoder = helper 0 (BS.empty, [], [])
    where
        helper o out []     inp = Done inp o out
        helper o out (t:ts) inp =
            case t of
                 TestGetFail e   -> Fail inp o e
                 TestGetOffset   -> helper o (out <> (BS.empty, [], [o])) ts inp
                 TestGetData i j ->
                     case splitRaw i j inp of
                          Nothing                -> Partial undefined
                          Just (Raw bs fs, inpB) -> helper (o + i) (out <> (bs, fs, [])) ts inpB

-- | Generate a random parser and check that it produces the correct output.
prop_get :: [TestGet] -> [Raw] ->  Property
prop_get ts is =
    foldl pushInput (runIncremental (makeGets ts)) is === expectedDecoder ts (mconcat is)

-- | Test that 'getWord32' works correctly.
prop_getWord32 :: Word32 -> Property
prop_getWord32 w =
    ioProperty . alloca $ \ptr -> do
        poke ptr w
        bs <- BS.packCStringLen (castPtr ptr, 4)
        return $ pushInput (runIncremental getWord32) (Raw bs []) === Done mempty 4 w

-- | Test that 'getAlign' works correctly.
prop_getAlign :: Int -> Int -> Raw -> Property
prop_getAlign i j inp =
    (i >= 0 && j >= 1) ==>
    pushInput (runIncremental (getSkipBytes i >> getAlign j >> getOffset)) inp === expected
    where
        n = ((i + (j - 1)) `div` j) * j
        expected =
            case splitRaw n 0 inp of
                 Nothing        -> Partial undefined
                 Just (_, rest) -> Done rest n n

return []
getTests :: IO Bool
getTests = $quickCheckAll
