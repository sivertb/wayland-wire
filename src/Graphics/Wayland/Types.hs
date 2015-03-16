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
    )
where

import Data.Int
import Data.Word

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

-- | Converts an 'ObjId' to a 'NewId'.
newFromObj :: ObjId -> NewId
newFromObj = NewId . unObjId
