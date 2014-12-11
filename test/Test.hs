module Test where

import Test.QuickCheck (quickCheck, forAll, listOf, arbitrary, Positive(..))
import Data.HaskID (encode, default_settings, decode)

round_trip_prop :: [Positive Int] -> Bool
round_trip_prop xs = decode default_settings (encode default_settings xs') == xs'
    where xs' = map (\(Positive x) -> x) xs
