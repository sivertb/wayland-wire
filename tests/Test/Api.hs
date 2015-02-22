{-# LANGUAGE TemplateHaskell #-}
module Test.Api
    ( apiTests )
where

import Control.Applicative
import Test.Arbitrary ()
import Test.Api.Gen
import qualified Test.Api.Client as C
import qualified Test.Api.Server as S
import Test.QuickCheck

$(genTests 'quickCheckResult)

apiTests :: IO Bool
apiTests = and . map checkResult <$> mapM execTest allTests

execTest :: (String, IO Result) -> IO Result
execTest (name, test) = putStrLn ("=== " ++ name ++ " ===") *> test <* putStrLn ""

checkResult :: Result -> Bool
checkResult (Success {}) = True
checkResult _            = False
