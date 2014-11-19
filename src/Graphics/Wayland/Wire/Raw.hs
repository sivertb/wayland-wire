module Graphics.Wayland.Wire.Raw
    ( Raw (..)
    , splitRaw
    )
where

import Control.Monad
import Data.Monoid
import qualified Data.ByteString as BS
import System.Posix

-----------------------------------------------------------------------------
-- Raw data
-----------------------------------------------------------------------------

-- | Describes a combined data stream of a 'ByteString' and a list of 'Fd'.
data Raw = Raw BS.ByteString [Fd] deriving (Eq, Show)

instance Monoid Raw where
    mempty = Raw BS.empty []
    mappend (Raw bA fA) (Raw bB fB) = Raw (bA `BS.append` bB) (fA ++ fB)

-- | Tries to split an 'Raw' into two streams. If the data is too short
-- 'Nothing' is returned.
splitRaw :: Int   -- ^ The number of bytes to take.
         -> Int   -- ^ The number of file descriptors to take.
         -> Raw   -- ^ The data to split.
         -> Maybe (Raw, Raw)
splitRaw i j (Raw bs fs) = do
    guard (BS.length bs >= i && length fs >= j)
    return (Raw bsA fsA, Raw bsB fsB)
    where
        (bsA, bsB) = BS.splitAt i bs
        (fsA, fsB) = splitAt j fs
