{-
Module      : Graphics.Wayland.Wire.Get
Description : Contains a parser similar to the one used for "Data.Binary.Get",
              except with two input streams.
Copyright   : (C) Sivert Berg, 2014
License     : GPL
Maintainer  : Sivert Berg <code@trev.is>
Stability   : Experimental

The Wayland protocol sends file descriptors in a different channel from other
data. This means the parser provided with "Data.Binary.Get" is insufficient, as
it only has a single input stream. This module defines a parser that has two
input streams, one used for normal message data, the second for file
descriptors.
-}
module Graphics.Wayland.Wire.Get
    (
    -- * The Get monad
      Get
    , Offset
    , Decoder (..)
    , runIncremental
    , pushInput
    -- * Primitive parsers
    , getData
    , getOffset
    , getFail
    -- * Utility parsers
    , getFds
    , getFd
    , getBytes
    , getWord32
    , getSkipBytes
    , getAlign
    )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Free
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Monoid
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Graphics.Wayland.Wire.Raw
import System.Posix
import Text.Printf

type Offset = Int

-----------------------------------------------------------------------------
-- Decoder
-----------------------------------------------------------------------------

-- | The result from 'runIncremental'.
data Decoder a
    = Done Raw Offset a           -- ^ The parser succeeded. Includes unused input.
    | Fail Raw Offset String      -- ^ The parser failed. Includes unused input.
    | Partial (Raw -> Decoder a)  -- ^ The parser needs more data to continue.

data GetF a =
    GetOffset (Offset -> a)
  | GetData Int Int (Raw -> a)
  | Failure String

instance Show a => Show (Decoder a) where
    show (Done i o a) = printf "Done (%s) %s (%s)" (show i) (show o) (show a)
    show (Fail i o e) = printf "Fail (%s) %s (%s)" (show i) (show o) (show e)
    show (Partial _)  = "Partial <func>"

instance Functor GetF where
    fmap f v =
        case v of
             GetOffset g   -> GetOffset (f . g)
             GetData i j g -> GetData i j (f . g)
             Failure e     -> Failure e

type Get a = FreeT GetF (State (Raw, Offset)) a

-- | Pushes more data to a decoder.
pushInput :: Decoder a -> Raw -> Decoder a
pushInput (Done i o a) j = Done (i <> j) o a
pushInput (Fail i o e) j = Fail (i <> j) o e
pushInput (Partial  p) i = p i

-- | Runs a 'Get' computation in the 'State' monad. The state contains the
-- current input and byte offset.
runGetF :: Get a -> State (Raw, Offset) (Decoder a)
runGetF g = do
    v <- runFreeT g
    case v of
         Pure a               -> (\(i, o) -> Done i o a) <$> get
         Free (Failure   e)   -> (\(i, o) -> Fail i o e) <$> get
         Free (GetOffset f)   -> gets snd >>= runGetF . f
         Free (GetData i j f) -> do
             (curInp, off) <- get
             case splitRaw i j curInp of
                  Just (inpA, inpB) -> put (inpB, off + i) >> runGetF (f inpA)
                  Nothing           -> return . Partial $ \newInp -> runGet (curInp <> newInp, off) g

-- | Runs a 'Get' computation given input and the current offset.
runGet :: (Raw, Offset) -> Get a -> Decoder a
runGet s = flip evalState s . runGetF

-- | Runs a 'Get' computation, returning a 'Decoder' value that can be used to
-- look at the result.
runIncremental :: Get a -> Decoder a
runIncremental = runGet (mempty, 0)


-----------------------------------------------------------------------------
-- Primitives. All parser are built from these functions.
-----------------------------------------------------------------------------

-- | Creates a parser that returns input of the specified size.
getData :: Int  -- ^ The number of bytes to read.
        -> Int  -- ^ The number of file descriptors to read.
        -> Get Raw
getData i j = liftF $ GetData i j id

-- | Creates a parser that returns the current byte offset into the stream.
getOffset :: Get Int
getOffset = liftF $ GetOffset id

-- | Causes the parsing to fail with the given error message.
getFail :: String -> Get a
getFail = liftF . Failure


-----------------------------------------------------------------------------
-- Handy parsers
-----------------------------------------------------------------------------

-- | Creates a parser that gets the specified number of file descriptors.
getFds :: Int -> Get [Fd]
getFds n = (\(Raw _ fs) -> fs) <$> getData 0 n

-- | Creates a parser that returns a single file descriptor.
getFd :: Get Fd
getFd = head <$> getFds 1

-- | Gets a specified number of bytes.
getBytes :: Int -> Get BS.ByteString
getBytes n = (\(Raw bs _) -> bs) <$> getData n 0

-- | Reads n bytes, then uses 'peek' to get a value from them.
getPtr :: Storable a => Int -> Get a
getPtr n = do
    bs <- getBytes n
    return (BS.inlinePerformIO $ BS.unsafeUseAsCString bs (peek . castPtr))

-- | Parses a 32bit word in host order.
getWord32 :: Get Word32
getWord32 = getPtr (sizeOf (undefined :: Word32))

-- | Skips the given number of bytes.
getSkipBytes :: Int -> Get ()
getSkipBytes = void . getBytes

-- | Skips bytes until the offset is aligned on the specified boundary.
getAlign :: Int -> Get ()
getAlign a = getOffset >>= getSkipBytes . f
    where
        f o = (a - o) `mod` a
