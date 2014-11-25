import Control.Applicative
import Control.Monad
import System.Exit
import Test.Get
import Test.Message

tests :: [IO Bool]
tests = [ getTests
        , messageTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
