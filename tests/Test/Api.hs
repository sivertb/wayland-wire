{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Api
    ( apiTests )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Word
import Graphics.Wayland.Dispatch
import Graphics.Wayland.ObjectManager
import Graphics.Wayland.Types
import Graphics.Wayland.W
import Prelude
import Test.Arbitrary ()
import Test.Api.Gen
import qualified Test.Api.Client as C
import qualified Test.Api.Server as S
import Test.QuickCheck

$(genTests 'quickCheckResult)

generatedTests :: IO Bool
generatedTests = all checkResult <$> mapM execTest allTests

execTest :: (String, IO Result) -> IO Result
execTest (name, test) = putStrLn ("=== " ++ name ++ " ===") *> test <* putStrLn ""

checkResult :: Result -> Bool
checkResult (Success {}) = True
checkResult _            = False

-- | Checks that a new object is created when calling a request taking a new_id.
prop_newReq :: Word32 -> Property
prop_newReq w =
    let (mgrA, mgrB, _, new, [], []) = runTest newObjectManager newObjectManager server1 client1
        (_   , _   , _, _  , resServer, resClient) = runTest mgrB mgrA server2 (client2 new)
    in resServer === [w] .&&. resClient === []
    where
        server1 msg = do
            let obj = Object 1
            registerObject
                obj
                S.OneArgumentSlots
                { S.oneArgumentReqNewId = \cons -> cons (\_ -> return undefined) >> return () }

            dispatchMessage msg

        client1 = do
            obj  <- objectFromNewId <$> allocObject
            C.oneArgumentReqNewId
                (signals obj)
                (\_ -> return C.OneArgumentSlots { C.oneArgumentEvtUint = lift . testResult } )

        server2 msg = dispatchMessage msg
        client2 new = S.oneArgumentEvtUint (signals (Object (unObject new))) w

instance Arbitrary C.OneArgumentEnum where
    arbitrary = elements [ C.OneArgumentEnumZero
                         , C.OneArgumentEnumOne
                         , C.OneArgumentEnumThree
                         ]

-- | Checks that it's possible to pass an Enum to a signal.
prop_enum :: ObjId -> C.OneArgumentEnum -> Property
prop_enum obj e =
    let (_, _, _, _, resServer, resClient) = runTest newObjectManager newObjectManager server client
    in resServer === [toWord32 e] .&&. resClient === []
    where
        server msg = do
            registerObject (Object obj) S.OneArgumentSlots { S.oneArgumentReqUint = lift . testResult }
            dispatchMessage msg

        client = C.oneArgumentReqUint (signals $ Object obj) e

return []
apiTests :: IO Bool
apiTests =
    (&&)
    <$> $quickCheckAll
    <*> generatedTests
