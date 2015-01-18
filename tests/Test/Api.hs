{-# LANGUAGE TemplateHaskell #-}
module Test.Api
    ( apiTests )
where

import Test.Api.Client
import Test.Api.Server
import Test.QuickCheck

return []
apiTests :: IO Bool
apiTests = $quickCheckAll
