import Control.Applicative
import Control.Monad
import System.Exit
import Test.Get
import Test.Wire

tests :: [IO Bool]
tests = [ getTests
        , wireTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
