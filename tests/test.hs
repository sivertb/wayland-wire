import Control.Applicative
import Control.Monad
import System.Exit
import Test.Encode
import Test.Get
import Test.Message
import Test.Socket

tests :: [IO Bool]
tests = [ getTests
        , messageTests
        , socketTests
        , encodeTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
