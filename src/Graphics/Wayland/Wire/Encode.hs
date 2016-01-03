{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Wayland.Wire.Encode
    ( Encodable
    , Decodable
    , ArgType
    , toMessage
    , fromMessage
    )
where

import Control.Monad
import Data.Int
import Data.Word
import Data.Monoid
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import System.Posix

class Encodable a where
    encode :: OpCode -> ObjId -> [MsgArg] -> a

class Decodable a b m where
    decode :: [MsgArg] -> Maybe String -> a -> m (Either String b)

instance Encodable Message where
    encode op obj = Message op obj . reverse

instance (ArgType a, Encodable b) => Encodable (a -> b) where
    encode op obj args a = encode op obj (toArg a : args)

instance Monad m => Decodable (m a) a m where
    decode args err m =
        case (args, err) of
             ([], Nothing) -> liftM Right m
             (_ , Nothing) -> return $ Left "Too many arguments"
             (_ , Just e ) -> return $ Left e

instance (ArgType a, Decodable b c m) => Decodable (a -> b) c m where
    decode []     err m = decode [] (err <> Just "Not enough arguments") (m undefined)
    decode (a:as) err m =
        case fromArg a of
             Nothing -> decode as (err <> Just "Argument is wrong type") (m undefined)
             Just x  -> decode as err (m x)

class ArgType a where
    toArg   :: a -> MsgArg
    fromArg :: MsgArg -> Maybe a

instance ArgType String where
    toArg = ArgString . Just
    fromArg (ArgString s) = s
    fromArg _             = Nothing

instance ArgType (Maybe String) where
    toArg = ArgString
    fromArg (ArgString s) = Just s
    fromArg _             = Nothing

instance ArgType Int32 where
    toArg = ArgInt
    fromArg (ArgInt i) = Just i
    fromArg _          = Nothing

instance ArgType Int where
    toArg = ArgInt . fromIntegral
    fromArg (ArgInt i) = Just $ fromIntegral i
    fromArg _          = Nothing

instance ArgType Word32 where
    toArg = ArgWord
    fromArg (ArgWord w) = Just w
    fromArg _           = Nothing

instance ArgType Fd where
    toArg = ArgFd
    fromArg (ArgFd f) = Just f
    fromArg _         = Nothing

instance ArgType ObjId where
    toArg = ArgObject . Just
    fromArg (ArgObject o) = o
    fromArg _             = Nothing

instance ArgType (Maybe ObjId) where
    toArg = ArgObject
    fromArg (ArgObject o) = Just o
    fromArg _             = Nothing

instance ArgType NewId where
    toArg = ArgNew . Just
    fromArg (ArgNew o) = o
    fromArg _          = Nothing

instance ArgType (Maybe NewId) where
    toArg = ArgNew
    fromArg (ArgNew o) = Just o
    fromArg _          = Nothing

instance ArgType Double where
    toArg = ArgFixed . doubleToFixed
    fromArg (ArgFixed f) = Just $ fixedToDouble f
    fromArg _            = Nothing

instance ArgType [Word32] where
    toArg = ArgArray
    fromArg (ArgArray a) = Just a
    fromArg _            = Nothing

toMessage :: Encodable a => OpCode -> ObjId -> a
toMessage op obj = encode op obj []

fromMessage :: (Monad m, Decodable a b m) => Message -> a -> m (Either String b)
fromMessage msg = decode (msgArgs msg) Nothing
