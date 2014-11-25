{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Wire.Put
    ( Put
    , PutM
    , runPut
    , putData
    , putBytes
    , putFds
    , putFd
    , putWord8
    , putWord32
    )
where

import Control.Applicative
import Control.Monad.Writer
import qualified Data.ByteString as BS
import Data.Word
import Foreign
import Graphics.Wayland.Wire.Raw
import System.IO.Unsafe
import System.Posix

newtype PutM a = Put { runPutM :: Writer (Endo Raw) a }
    deriving (Functor, Applicative, Monad, MonadWriter (Endo Raw))

type Put = PutM ()

runPut :: Put -> Raw
runPut = fromEndo . execWriter . runPutM

fromEndo :: Monoid a => Endo a -> a
fromEndo e = appEndo e mempty

putData :: Raw -> Put
putData = tell . Endo . mappend

putBytes :: BS.ByteString -> Put
putBytes bs = putData $ Raw bs []

putFds :: [Fd] -> Put
putFds fds = putData $ Raw BS.empty fds

putFd :: Fd -> Put
putFd = putFds . (: [])

putWord8 :: Word8 -> Put
putWord8 = putBytes . BS.singleton

putWord32 :: Word32 -> Put
putWord32 w =
    putBytes
    . unsafePerformIO
    . with w
    $ \ptr -> BS.packCStringLen (castPtr ptr, sizeOf w)
