module Main where

import Test.Tasty (defaultMain, testGroup)
import QuickChecks (qchecks)
import Cases (encode_cases)

main :: IO ()
main = defaultMain $ testGroup "Tests" [qchecks, encode_cases]
