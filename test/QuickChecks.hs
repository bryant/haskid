module QuickChecks where

import Test.Tasty.QuickCheck (forAll, listOf, arbitrary, Positive(..),
                              testProperty)
import Test.Tasty (testGroup)
import Data.HaskID (encode, haskid, decode)

round_trip_prop :: [Positive Int] -> Bool
round_trip_prop xs = decode haskid (encode haskid xs') == xs'
    where xs' = map getPositive xs

qchecks = testGroup "Properties" [testProperty "round trip" round_trip_prop]
