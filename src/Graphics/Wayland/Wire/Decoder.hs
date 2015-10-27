{-
Module      : Graphics.Wayland.Wire.Decoder
Description : Contains a decoder similar to the one used for "Data.Binary.Get".
              except with two input streams.
Copyright   : (C) Sivert Berg, 2014
License     : GPL
Maintainer  : Sivert Berg <code@trev.is>
Stability   : Experimental

The Wayland protocol sends file descriptors in a different channel from other
data. This means the decoder provided with "Data.Binary.Get" is insufficient,
as it only has a single input stream. This module defines a decoder that has
two input streams, one used for normal message data, the second for file
descriptors.
-}
module Graphics.Wayland.Wire.Decoder
    ( Decoder (..)
    , pushData
    , pushData'
    , pushEnd
    , pullFd
    , pullRestart
    , pullGet
    , pullGetOffset
    , pullFail
    , pullSkipBytes
    , pullSkipFds
    , pullRemaining
    , pullBytes
    , pullAlign
    , runDecoder'
    , runDecoder
    )
where

import Control.Applicative
import Control.Monad
import Data.Binary.Get hiding (Decoder, Partial, Done, Fail)
import qualified Data.Binary.Get as G
import qualified Data.ByteString as BS
import Prelude
import System.Posix
import Text.Printf

-- | The decoder data type.
data Decoder a =
    Fail !BS.ByteString !ByteOffset [Fd] String
  -- ^ The decoder failed, either by calling 'pullFail' or by being given
  -- insufficient data. It contains any unconsumed input data and an error
  -- message.
  | Done !BS.ByteString !ByteOffset [Fd] a
  -- ^ The decoder finished successfully. Contains any unconsumed input, as
  -- well as the return value.
  | Partial (Maybe (BS.ByteString, [Fd]) -> Decoder a)
  -- ^ The decoder needs more data to continue. More data can be added by
  -- calling the 'pushData' or 'pushData''.
  | Offset  (ByteOffset -> Decoder a)
  -- ^ The decoder wants the current byte offset, and will return a new decoder
  -- once it gets it.
  | Restart (Decoder a)
  -- ^ The offset counting should reset. The new decoder returned will start at
  -- offset 0.

instance Eq a => Eq (Decoder a) where
    a == b =
        case (a, b) of
             (Fail bsA offA fdsA errA, Fail bsB offB fdsB errB) ->
                 and [ bsA == bsB, offA == offB, fdsA == fdsB, errA == errB ]
             (Done bsA offA fdsA xA  , Done bsB offB fdsB xB  ) ->
                 and [ bsA == bsB, offA == offB, fdsA == fdsB, xA == xB ]
             _ -> False

instance Show a => Show (Decoder a) where
    show (Fail bs off fds msg) = printf "Fail %s %s %s %s" (show bs) (show off) (show fds) (show msg)
    show (Done bs off fds x  ) = printf "Done %s %s %s %s" (show bs) (show off) (show fds) (show x  )
    show (Partial _          ) = "Partial <func>"
    show (Offset  _          ) = "Offset <func>"
    show (Restart p          ) = "Restart (" ++ show p ++ ")"

instance Functor Decoder where
    fmap f (Offset  g)         = Offset  (fmap f . g)
    fmap f (Partial g)         = Partial (fmap f . g)
    fmap f (Restart p)         = Restart (fmap f p)
    fmap f (Done bs off fds a) = Done bs off fds (f a)
    fmap _ (Fail bs off fds e) = Fail bs off fds e

instance Applicative Decoder where
    pure = return
    (<*>) = ap

instance Monad Decoder where
    return = Done BS.empty 0 []

    (Restart p   )   >>= g = Restart (p >>= g)
    (Offset  p   )   >>= g = Offset  ((>>= g) . p)
    (Partial p   )   >>= g = Partial ((>>= g) . p)
    (Fail b o f s)   >>= _ = Fail b o f s
    d@(Done b o f a) >>= g =
        case g a of
             Restart p    -> Done b        0 f  () >> p
             Partial p    -> addOffset o $ p (Just (b, f))
             Done c p h x -> Done (b `BS.append` c) (o + p) (f ++ h) x
             Offset  p    -> d >> p o
             err          -> err

addOffset :: ByteOffset -> Decoder a -> Decoder a
addOffset o v =
    case v of
         Done a p f x -> Done a (o + p) f x
         Fail a p f e -> Fail a (o + p) f e
         Partial p    -> Partial (addOffset o . p)
         Offset  p    -> Offset  (p . (+ o))
         Restart p    -> Restart p

-- | Runs a Get parser several times, until it fails or the input ends.
getList :: Get a -> Get [a]
getList g = helper []
    where
        helper as = do
            e <- isEmpty
            if e
              then return $ reverse as
              else g >>= helper . (:as)

-- | Parses a 32bit word as a file descriptor.
getFd :: Get Fd
getFd = fromIntegral <$> getWord32host

-- | Parses a list of 32bit words as a list of file descriptors.
getFds :: Get [Fd]
getFds = getList getFd

-- | Runs a Get parser that returns a list on a ByteString.
runGetList :: Get [a] -> BS.ByteString -> [a]
runGetList g bs =
    case pushEndOfInput (runGetIncremental g `pushChunk` bs) of
         G.Done _ _ as -> as
         _             -> []

-- | Pushes more data to a decoder. The second stream has already been decoded
-- as file descriptors.
pushData' :: BS.ByteString -> [Fd] -> Decoder a -> Decoder a
pushData' b g v =
    case v of
         Done a o f x -> Done (a `BS.append` b) o (f ++ g) x
         Fail a o f e -> Fail (a `BS.append` b) o (f ++ g) e
         Partial p    -> p $ Just (b, g)
         Offset  o    -> Offset (pushData' b g . o)
         Restart p    -> Restart (pushData' b g p)

-- | Pushes more data to a decoder. The second input stream will be decoded as
-- file descriptors using 'getFds'.
pushData :: BS.ByteString -> BS.ByteString -> Decoder a -> Decoder a
pushData a = pushData' a . runGetList getFds

-- | Marks the end of input.
pushEnd :: Decoder a -> Decoder a
pushEnd v =
    case v of
         Partial p -> pushEnd $ p Nothing
         Offset  p -> pushEnd $ p 0
         Restart p -> pushEnd p
         _         -> v

-- | Pulls a file descriptor from the input stream.
pullFd :: Decoder Fd
pullFd = Partial (f BS.empty)
    where
        f as m =
            case m of
                 Nothing         -> Fail as 0 [] "pullFd failed, end of input"
                 Just (bs, [])   -> Partial . f $ as `BS.append` bs
                 Just (bs, g:gs) -> Done (as `BS.append` bs) 0 gs g

-- | Zeroes the byte counter.
pullRestart :: Decoder ()
pullRestart = Restart $ return ()

-- | Lifts a Get value into the Decoder monad.
-- This allows Get parsers to be reused.
pullGet :: Get a -> Decoder a
pullGet = wrapGet [] . runGetIncremental
    where
        -- Converts a function used with 'G.Partial' into a function that can be used with 'Partial'.
        wrapPartial :: [Fd] -> (Maybe BS.ByteString -> G.Decoder a) -> Maybe (BS.ByteString, [Fd]) -> Decoder a
        wrapPartial fds g m =
            case m of
                 Nothing        -> wrapGet fds          (g Nothing)
                 Just (bs, gds) -> wrapGet (fds ++ gds) (g $ Just bs)

        -- Wraps a 'G.Decoder' in a 'Decoder'.
        wrapGet :: [Fd] -> G.Decoder a -> Decoder a
        wrapGet fds d =
            case d of
                 G.Fail as off str  -> Fail as off fds str
                 G.Done as off a    -> Done as off fds a
                 G.Partial g        -> Partial $ wrapPartial fds g

-- | Gets the current offset in the stream.
pullGetOffset :: Decoder ByteOffset
pullGetOffset = Offset $ \off -> return off

-- | Causes the parsing to fail with an error message.
pullFail :: String -> Decoder a
pullFail = Fail BS.empty 0 []

-- | Discards a number of bytes in the stream.
pullSkipBytes :: ByteOffset -> Decoder ()
pullSkipBytes = pullGet . skip . fromIntegral

-- | Discards a number of file descriptors.
pullSkipFds :: Int -> Decoder ()
pullSkipFds = flip replicateM_ pullFd

-- | Gets a certain number of bytes.
pullBytes :: Int -> Decoder BS.ByteString
pullBytes = pullGet . getByteString

-- | Gets the remaining input.
pullRemaining :: Decoder (BS.ByteString, [Fd])
pullRemaining = Partial $ f BS.empty 0 []
    where
        f bs off fds m =
            case m of
                 Nothing        -> addOffset (fromIntegral off) $ return (bs, fds)
                 Just (cs, gds) -> Partial $ f (bs `BS.append` cs) (off + BS.length cs) (fds ++ gds)

-- | Skips bytes until the number of bytes consumed is aligned on the given
-- value.
pullAlign :: ByteOffset -> Decoder ()
pullAlign align = do
    o <- pullGetOffset
    pullSkipBytes ((align - o) `mod` align)

-- | Runs a decoder on two input streams, where the second one has already been
-- parsed into file descriptors. It returns either an error or the result.
runDecoder' :: BS.ByteString -> [Fd] -> Decoder a -> Either String a
runDecoder' bs fs p =
    case pushEnd (pushData' bs fs p) of
         Done _ _ _ a -> Right a
         Fail _ _ _ e -> Left e
         _            -> Left "Something failed"
-- | Runs a decoder on two input streams, and returns either an error or the result.
runDecoder :: BS.ByteString -> BS.ByteString -> Decoder a -> Either String a
runDecoder bs fs p =
    case pushEnd (pushData bs fs p) of
         Done _ _ _ a -> Right a
         Fail _ _ _ e -> Left e
         _            -> Left "Something failed"
