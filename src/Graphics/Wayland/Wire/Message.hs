{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Wire.Message
    ( MessageLookup
    , Message (..)
    , Argument (..)
    , getMsg
    , putMsg
    , Fixed
    , fixedToDouble
    , doubleToFixed
    )
where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int
import Data.Word
import Graphics.Wayland.Protocol hiding (Argument)
import Graphics.Wayland.Wire.Get
import Graphics.Wayland.Wire.Put
import qualified Graphics.Wayland.Protocol as P
import Graphics.Wayland.Types
import System.Posix
import Text.Printf

-- | A function that is used to lookup the number and type of the arguments a
-- message will have based on the object id and op code.
type MessageLookup = ObjId -> OpCode -> Maybe [P.Type]

data Message =
    Message { msgOp   :: OpCode
            , msgObj  :: ObjId
            , msgArgs :: [Argument]
            }
    deriving (Show, Eq)

data Argument =
    ArgInt    Int32
  | ArgWord   Word32
  | ArgFixed  Fixed
  | ArgFd     Fd
  | ArgString (Maybe String)
  | ArgObject (Maybe ObjId)
  | ArgNew    (Maybe ObjId)
  | ArgArray  [Word32]
  deriving (Show, Eq)

newtype Fixed = Fixed { unFixed :: Int32 } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show Fixed where
    show (Fixed i) = let (q,r) = quotRem i 256 in printf "%d.%u" q (abs r)

getInt32 :: Get Int32
getInt32 = fromIntegral <$> getWord32

getFixed :: Get Fixed
getFixed = Fixed <$> getInt32

getArray :: Get [Word32]
getArray = do
    size <- getWord32
    replicateM (fromIntegral $ size `div` 4) getWord32

getString :: Get (Maybe String)
getString = do
    len <- getWord32
    if len == 0
      then return Nothing
      else do
          str <- getBytes $ fromIntegral len - 1
          getSkipBytes 1
          getAlign 4
          return . Just $ UTF8.toString str

getObjId :: Get (Maybe ObjId)
getObjId = let f 0 = Nothing
               f x = Just x
           in  f <$> getWord32

getArg :: P.Type -> Get Argument
getArg t =
    case t of
         TypeSigned     -> ArgInt    <$> getInt32
         TypeUnsigned   -> ArgWord   <$> getWord32
         TypeArray      -> ArgArray  <$> getArray
         TypeString _   -> ArgString <$> getString
         TypeObject _ _ -> ArgObject <$> getObjId
         TypeNew    _ _ -> ArgNew    <$> getObjId
         TypeFd         -> ArgFd     <$> getFd
         TypeFixed      -> ArgFixed  <$> getFixed

fixedToDouble :: Fixed -> Double
fixedToDouble = (/ 256) . fromIntegral . unFixed

doubleToFixed :: Double -> Fixed
doubleToFixed = Fixed . round . (* 256)

getMsg :: MessageLookup -> Get Message
getMsg lf = do
    senderId <- getWord32
    word2    <- getWord32
    let size  = word2 `shiftR` 16
        op    = fromIntegral $ word2 .&. 0xffff
        margs = lf senderId op

    args <- case margs of
                 Just as -> mapM getArg as
                 Nothing -> getFail $ printf "Unknown object %i while parsing message" senderId

    off <- getOffset
    unless (off == fromIntegral size)
        . getFail
        $ printf "Received message with wrong size. Expected %i bytes, got %i bytes" size off

    return $ Message op senderId args

argLength :: Argument -> Word32
argLength arg =
    case arg of
         ArgString s -> fromIntegral $ maybe 4 strLen s
         ArgArray  a -> fromIntegral $ arrLen a
         ArgFd     _ -> 0
         _           -> 4
    where
        strLen s  = align 4 $ 4 + BS.length (UTF8.fromString s) + 1
        arrLen as = 4 + 4 * length as
        align a x = x + ((a - x) `mod` a)

-- | Calculates the length in bytes a message will take when encoded as a
-- ByteString. It is the length of the arguments plus 8 bytes for the header.
msgLength :: Message -> Word32
msgLength = (8 +) . sum . map argLength . msgArgs

-- | Pads the message with 0s until it is aligned on a 4-byte boundary.
putPadding :: Int -> Put
putPadding i
    | i `mod` 4 == 0 = return ()
    | otherwise      = putWord8 0 >> putPadding (i + 1)

putString :: String -> Put
putString s = do
    let bs  = UTF8.fromString s
        len = BS.length bs + 1
    putWord32 $ fromIntegral len
    putBytes bs
    putWord8 0
    putPadding len

putArray :: [Word32] -> Put
putArray as = do
    putWord32 . fromIntegral $ length as * 4
    mapM_ putWord32 as

putArg :: Argument -> Put
putArg arg =
    case arg of
         ArgWord   u -> putWord32 u
         ArgInt    i -> putWord32 $ fromIntegral i
         ArgFixed  f -> putWord32 . fromIntegral $ unFixed f
         ArgFd     f -> putFd f
         ArgString s -> maybe (putWord32 0) putString s
         ArgObject o -> maybe (putWord32 0) putWord32 o
         ArgNew    o -> maybe (putWord32 0) putWord32 o
         ArgArray  a -> putArray a

-- | Serializes everything but the file descriptor arguments of the message.
putMsg :: Message -> Put
putMsg msg = do
    let len   = msgLength msg
        word2 = (len `shiftL` 16) .|. fromIntegral (msgOp msg .&. 0xffff)
    putWord32 $ msgObj msg
    putWord32 word2
    mapM_ putArg $ msgArgs msg
