import Control.Applicative
import Control.Monad
import Prelude
import System.Exit
import Test.Api
import Test.Encode
import Test.Get
import Test.Message
import Test.Socket

tests :: [IO Bool]
tests = [ apiTests
        , getTests
        , messageTests
        , socketTests
        , encodeTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
