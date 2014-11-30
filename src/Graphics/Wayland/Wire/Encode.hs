{-# LANGUAGE FlexibleInstances #-}
module Graphics.Wayland.Wire.Encode
    ( Encodable
    , Decodable
    , ArgType
    , toMessage
    , fromMessage
    )
where

import Data.Int
import Data.Word
import Data.Monoid
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Message
import System.Posix

class Encodable a where
    encode :: OpCode -> ObjId -> [Argument] -> a

class Decodable a where
    decode :: [Argument] -> Maybe String -> a -> IO (Maybe String)

instance Encodable Message where
    encode op obj = Message op obj . reverse

instance (ArgType a, Encodable b) => Encodable (a -> b) where
    encode op obj args a = encode op obj (toArg a : args)

instance Decodable (IO a) where
    decode args err m =
        case (args, err) of
             ([], Nothing) -> m >> return Nothing
             ([], _      ) -> return err
             (_ , _      ) -> return (err <> Just "Too many arguments")

instance (ArgType a, Decodable b) => Decodable (a -> b) where
    decode []     err m = decode [] (err <> Just "Not enough arguments") (m undefined)
    decode (a:as) err m =
        case fromArg a of
             Nothing -> decode as (err <> Just "Argument is wrong type") (m undefined)
             Just x  -> decode as err (m x)

class ArgType a where
    toArg   :: a -> Argument
    fromArg :: Argument -> Maybe a

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

fromMessage :: Decodable a => Message -> a -> IO (Maybe String)
fromMessage msg = decode (msgArgs msg) Nothing
