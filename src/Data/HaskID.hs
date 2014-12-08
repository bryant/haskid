module Data.HaskID where

import Data.Char (ord)
import Data.List (foldl', zipWith4)

shuffle :: String -> String -> String
shuffle xs = foldl' swap xs . mk_swap_points (length xs)

mk_swap_points :: Int -> String -> [(Int, Int)]
mk_swap_points start salt =
    zipWith3 compute sumsalt [start - 1, start - 2..1] salts
    where
    salts = cycle . zip [0..] $ map ord salt
    sumsalt = scanl1 (+) $ map snd salts
    compute p i (v, integer) = (i, (integer + v + p) `rem` i)

swap :: [a] -> (Int, Int) -> [a]
swap xs (i, j)
    | i == j = xs
    | j < i = swap xs (j, i)
    | otherwise = seg xs 0 i ++ [xs !! j] ++ seg xs (i + 1) j ++ [xs !! i]
                ++ seg xs (j + 1) (length xs)

seg :: [a] -> Int -> Int -> [a]
seg xs start end = take (end - start) (drop start xs)
