{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Types
    ( ObjId (..)
    , NewId (..)
    , OpCode (..)
    )
where

import Data.Word

newtype ObjId = ObjId { unObjId :: Word32 }
    deriving (Show, Eq, Ord, Num)
newtype NewId = NewId { unNewId :: Word32 }
    deriving (Show, Eq, Ord, Num)
newtype OpCode = OpCode { unOpCode :: Word16 }
    deriving (Show, Eq, Ord, Num)
