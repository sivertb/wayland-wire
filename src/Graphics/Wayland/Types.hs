{-|
Module      : Graphics.Wayland.Types
Description : Various types used by the other modules
Copyright   : (C) Sivert Berg, 2014-2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Types
    ( ObjId (..)
    , NewId (..)
    , OpCode (..)
    , WireEnum (..)
    , newFromObj

    , Bitfield
    , fromBitfield
    , toBitfield
    )
where

import Control.Arrow
import Data.Bits
import Data.Foldable
import Data.Int
import Data.Word
import qualified Data.Set as S

newtype ObjId = ObjId { unObjId :: Word32 }
    deriving (Show, Eq, Ord, Enum, Num)
newtype NewId = NewId { unNewId :: Word32 }
    deriving (Show, Eq, Ord, Enum, Num)
newtype OpCode = OpCode { unOpCode :: Word16 }
    deriving (Show, Eq, Ord, Enum, Num)

class WireEnum w where
    fromWord32 :: Word32 -> Maybe w
    fromWord32 = fromInt32 . fromIntegral

    toWord32 :: w -> Word32
    toWord32 = fromIntegral . toInt32

    fromInt32 :: Int32 -> Maybe w
    fromInt32 = fromWord32 . fromIntegral

    toInt32 :: w -> Int32
    toInt32 = fromIntegral . toWord32

instance WireEnum Word32 where
    fromWord32 = Just
    toWord32 = id

instance WireEnum Int32 where
    fromInt32 = Just
    toInt32 = id

-- | Represents bitfield enums.
newtype Bitfield a = Bitfield { unBitfield :: S.Set a }
    deriving (Eq, Show)

instance (Ord a, Bounded a, Enum a, WireEnum a) => WireEnum (Bitfield a) where
    toWord32 = foldl' (.|.) 0 . map toWord32 . fromBitfield
    fromWord32 w =
        Just
        . toBitfield
        . map snd
        . filter ((/= 0) . fst)
        $ map ((.&. w) . toWord32 &&& id) [minBound .. maxBound]

instance (Ord a, Enum a, Bounded a) => Bounded (Bitfield a) where
    minBound = toBitfield []
    maxBound = toBitfield [minBound .. maxBound]

instance (Ord a, Enum a, Bounded a) => Enum (Bitfield a) where
    toEnum i = toBitfield . map snd . filter fst $ map (testBit i . fromEnum &&& id) [minBound .. maxBound]
    fromEnum = sum . map ((2^) . fromEnum) . fromBitfield

-- | Converts a set of bits into a 'Bitfield'.
toBitfield :: Ord a => [a] -> Bitfield a
toBitfield = Bitfield . S.fromList

-- | Converts from a 'Bitfield' into the individual bits.
fromBitfield :: Ord a => Bitfield a -> [a]
fromBitfield = S.toList . unBitfield

-- | Converts an 'ObjId' to a 'NewId'.
newFromObj :: ObjId -> NewId
newFromObj = NewId . unObjId
