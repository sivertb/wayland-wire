{-# LANGUAGE TemplateHaskell #-}
module Test.Decoder
    ( decoderTests )
where

import Control.Applicative
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Get as G
import Data.Binary.Put
import Graphics.Wayland.Wire.Decoder
import System.Posix
import Test.QuickCheck

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink = map BS.pack . shrink . BS.unpack

instance Arbitrary Fd where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word32)

runPut' :: Put -> BS.ByteString
runPut' = BSL.toStrict . runPut

-- | Check that running a failing decoder returns the error message.
prop_failRun :: BS.ByteString -> [Fd] -> String -> Bool
prop_failRun bs fds msg = Left msg == runDecoder' bs fds ((pullFail msg) :: Decoder ())

-- | Checks that pushing data to a Fail decoder will keep the pushed data.
prop_failPush :: BS.ByteString -> [Fd] -> String -> Property
prop_failPush bs fds msg = Fail bs 0 fds msg === pushData' bs fds (return () >> pullFail msg :: Decoder ())

-- | Check that pushing data to a Done decoder will keep the pushed data.
prop_donePush :: BS.ByteString -> [Fd] -> Property
prop_donePush bs fds = Done bs 0 fds () === pushData' bs fds (return ())

-- | Check that pushing more than once to a decoder keeps the data in the correct order.
prop_donePushMany :: [(BS.ByteString, [Fd])] -> Property
prop_donePushMany input = Done bs 0 fds () === foldl (\p (b, f) -> pushData' b f p) (return ()) input
    where
        bs  = BS.concat $ map fst input
        fds = concat    $ map snd input

-- | Check that '(>>=)' works correct when combining two 'Done' decoders.
prop_bindDone :: BS.ByteString -> [Fd] -> BS.ByteString -> [Fd] -> Property
prop_bindDone bA fA bB fB =
    Done (bA `BS.append` bB) 0 (fA ++ fB) () === (pushData' bA fA (return ()) >> pushData' bB fB (return ()))

-- | Check that '(>>=)' correctly combines two partial decoders with no data.
prop_bindPartial1 :: BS.ByteString -> [Fd] -> Property
prop_bindPartial1 bs fds =
    forAll (choose (0, BS.length bs )) $ \nBytes ->
    forAll (choose (0, length    fds)) $ \nFds   ->
        Done (BS.drop nBytes bs) (fromIntegral nBytes) (drop nFds fds) ()
        === pushData' bs fds (pullSkipBytes (fromIntegral nBytes) >> pullSkipFds nFds)

-- | Check that '(>>=)' correctly combines with a partial decoder that have
-- eaten some data.
prop_bindPartial2 :: BS.ByteString -> [Fd] -> BS.ByteString -> [Fd] -> Property
prop_bindPartial2 bsA fdsA bsB fdsB =
    Done BS.empty off [] (bsA `BS.append` bsB, fdsA ++ fdsB)
    === pushEnd (pushData' bsB fdsB (return ()) >> pushData' bsA fdsA pullRemaining)
    where
        off = fromIntegral $ BS.length bsA + BS.length bsB

prop_putGet :: Word32 -> Property
prop_putGet w = Right w === runDecoder (runPut' $ putWord32host w) BS.empty (pullGet G.getWord32host)

prop_putGetPartial :: Word32 -> Property
prop_putGetPartial w =
    Done BS.empty 4 [] w === foldl (\p b -> pushData' (BS.pack [b]) [] p) (pullGet G.getWord32host) input
    where
        input = BS.unpack . runPut' $ putWord32host w

prop_pullFd :: Fd -> Property
prop_pullFd f = Right f === runDecoder' BS.empty [f] pullFd

prop_pullGetOffset :: BS.ByteString -> Property
prop_pullGetOffset bs =
    forAll (choose (0, len)) $ \off ->
        Done (BS.drop (fromIntegral off) bs) off [] off === pushData' bs [] (pullSkipBytes off >> pullGetOffset)
    where
        len = fromIntegral $ BS.length bs

return []
decoderTests :: IO Bool
decoderTests = $quickCheckAll
