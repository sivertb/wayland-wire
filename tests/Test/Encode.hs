{-# LANGUAGE TemplateHaskell #-}
module Test.Encode
where

import Data.Int
import Data.Word
import Graphics.Wayland.Types
import Graphics.Wayland.Wire.Encode
import Graphics.Wayland.Wire.Message
import System.Posix
import Test.Message ()
import Test.QuickCheck

-- | Tests that toMessage creates a correct message.
prop_encode :: OpCode -> ObjId -> Int32 -> String -> Maybe ObjId -> Fd -> NewId -> [Word32] -> Property
prop_encode op obj i s mo f n a =
    toMessage op obj i s mo f n a ===
        Message op obj [ ArgInt i
                       , ArgString (Just s)
                       , ArgObject mo
                       , ArgFd f
                       , ArgNew (Just n)
                       , ArgArray a
                       ]

-- | Tests that fromMessage correctly parses a message
prop_decode :: Int32 -> String -> Maybe ObjId -> Fd -> NewId -> [Word32] -> Property
prop_decode i s mo f n a =
    ioProperty $ do
        dat  <- fromMessage msg func
        return $ dat === (i, s, mo, f, n, a)
    where
        func :: Int32 -> String -> Maybe ObjId -> Fd -> NewId -> [Word32]
             -> IO (Int32, String, Maybe ObjId, Fd, NewId, [Word32])
        func i s mo f n a = return (i, s, mo, f, n, a)
        msg = Message 0 0 [ ArgInt i
                          , ArgString (Just s)
                          , ArgObject mo
                          , ArgFd f
                          , ArgNew (Just n)
                          , ArgArray a
                          ]

return []
encodeTests :: IO Bool
encodeTests = $quickCheckAll
