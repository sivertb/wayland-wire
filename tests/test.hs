import Control.Applicative
import Control.Monad
import System.Exit
import Test.Decoder

tests :: [IO Bool]
tests = [ decoderTests ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
