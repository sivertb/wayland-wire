import Control.Applicative
import Control.Monad
import System.Exit
import Test.Get
import Test.Message
import Test.Socket

tests :: [IO Bool]
tests = [ getTests
        , messageTests
        , socketTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
