module Graphics.Wayland.Wire.Message
    ( Message (..)
    , Argument (..)
    , pullMsg
    , putMsg
    , putCmsg
    )
where

import Control.Applicative
import Control.Monad
import Data.Binary.Get hiding (Decoder)
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe
import Data.Int
import Data.Word
import Graphics.Wayland.Protocol hiding (Argument)
import Graphics.Wayland.Wire.Decoder
import qualified Graphics.Wayland.Protocol as P
import Graphics.Wayland.Types
import System.Posix
import Text.Printf

data Message =
    Message { msgOp   :: OpCode
            , msgObj  :: ObjId
            , msgArgs :: [Argument]
            }
    deriving (Show, Eq)

data Argument =
    ArgInt    Int32
  | ArgWord   Word32
  | ArgFixed  Double
  | ArgFd     Fd
  | ArgString (Maybe String)
  | ArgObject (Maybe ObjId)
  | ArgNew    (Maybe ObjId)
  | ArgArray  [Word32]
  deriving (Show, Eq)


pullWord32 :: Decoder Word32
pullWord32 = pullGet getWord32host

pullInt32 :: Decoder Int32
pullInt32 = fromIntegral <$> pullGet getWord32host

pullArray :: Decoder [Word32]
pullArray = do
    size <- pullWord32
    replicateM (fromIntegral $ size `div` 4) pullWord32

pullString :: Decoder (Maybe String)
pullString = do
    len <- pullWord32
    if len == 0
      then return Nothing
      else do
          str <- pullBytes $ fromIntegral len - 1
          pullSkipBytes 1
          pullAlign 4
          return . Just $ UTF8.toString str

pullObjId :: Decoder (Maybe ObjId)
pullObjId = let f 0 = Nothing
                f x = Just x
            in  f <$> pullWord32

pullArg :: P.Argument -> Decoder Argument
pullArg arg =
    case argType arg of
         TypeSigned     -> ArgInt    <$> pullInt32
         TypeUnsigned   -> ArgWord   <$> pullWord32
         TypeArray      -> ArgArray  <$> pullArray
         TypeString _   -> ArgString <$> pullString
         TypeObject _ _ -> ArgObject <$> pullObjId
         TypeNew    _ _ -> ArgNew    <$> pullObjId
         TypeFd         -> ArgFd     <$> pullFd
         TypeFixed      -> ArgFixed . fixedToDouble <$> pullInt32
    where
        fixedToDouble :: Int32 -> Double
        fixedToDouble = (/ 256) . fromIntegral


pullMsg :: (ObjId -> OpCode -> Maybe [P.Argument]) -> Decoder Message
pullMsg lf = do
    pullRestart
    senderId <- pullWord32
    word2    <- pullWord32
    let size  = word2 `shiftR` 16
        op    = word2 .&. 0xffff
        margs = lf senderId op

    args <- case margs of
                 Just as -> mapM pullArg as
                 Nothing -> pullFail $ printf "Unknown object %i while parsing message" senderId

    off <- pullGetOffset
    unless (off == fromIntegral size)
        . pullFail
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
    putWord32host $ fromIntegral len
    putByteString bs
    putWord8 0
    putPadding len

putArray :: [Word32] -> Put
putArray as = do
    putWord32host . fromIntegral $ length as * 4
    mapM_ putWord32host as

putArg :: Argument -> Put
putArg arg =
    case arg of
         ArgWord   u -> putWord32host u
         ArgInt    i -> putWord32host $ fromIntegral i
         ArgFixed  d -> putWord32host $ doubleToFixed d
         ArgFd     _ -> return ()
         ArgString s -> maybe (putWord32host 0) putString s
         ArgObject o -> maybe (putWord32host 0) putWord32host o
         ArgNew    o -> maybe (putWord32host 0) putWord32host o
         ArgArray  a -> putArray a
    where
        doubleToFixed d = round (d * 256)

-- | Serializes everything but the file descriptor arguments of the message.
putMsg :: Message -> Put
putMsg msg = do
    let len   = msgLength msg
        word2 = (len `shiftL` 16) .|. (msgOp msg .&. 0xffff)
    putWord32host $ msgObj msg
    putWord32host word2
    mapM_ putArg $ msgArgs msg

-- | Serializes the file descriptor arguments of the message.
putCmsg :: Message -> Put
putCmsg = mapM_ putWord32host . mapMaybe findFd . msgArgs
    where
        findFd (ArgFd fd) = Just $ fromIntegral fd
        findFd _          = Nothing
