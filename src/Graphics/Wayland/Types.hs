{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Types
    ( ObjId (..)
    , NewId (..)
    , OpCode (..)
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

newFromObj :: ObjId -> NewId
newFromObj = NewId . unObjId
