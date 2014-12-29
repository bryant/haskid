module Main where

import Data.Word (Word64)
import Data.List (foldl')

import Data.HaskID (opts, encode, decode, HashOptions(..), init_haskid)

import Criterion.Main (defaultMain, bgroup, bench, nfIO)

foreign import ccall "round_trip_bench"
    round_trip_bench :: Word64 -> IO Word64

foreign import ccall "control"
    control :: Word64 -> IO Word64

nums :: [Int]
nums = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9]

main :: IO ()
main = defaultMain
    [ bgroup (show loops)
        [ bench "haskid" . nfIO . foldl' (>>=) (return nums) $
                                         replicate loops haskid_call
        , bench "hashidsxx" . nfIO . round_trip_bench $ fromIntegral loops
        , bench "control" . nfIO . control $ fromIntegral loops
        ]
    | loops <- [0, 20, 200, 2000, 20000]
    ]
    where
    haskid_call p = return . decode haskid $ encode haskid p
    Right haskid = init_haskid opts { opt_salt = "a salt."
                                    , opt_min_length = 25
                                    }
