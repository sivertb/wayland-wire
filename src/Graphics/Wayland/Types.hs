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
    , WordEnum (..)
    , newFromObj
    )
where

import Data.Word

newtype ObjId = ObjId { unObjId :: Word32 }
    deriving (Show, Eq, Ord, Enum, Num)
newtype NewId = NewId { unNewId :: Word32 }
    deriving (Show, Eq, Ord, Enum, Num)
newtype OpCode = OpCode { unOpCode :: Word16 }
    deriving (Show, Eq, Ord, Enum, Num)

class WordEnum w where
    toWordEnum :: Word32 -> Maybe w
    fromWordEnum :: w -> Word32

instance WordEnum Word32 where
    toWordEnum = Just
    fromWordEnum = id

-- | Converts an 'ObjId' to a 'NewId'.
newFromObj :: ObjId -> NewId
newFromObj = NewId . unObjId
