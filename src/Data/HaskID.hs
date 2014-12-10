module Data.HaskID where

import Data.Char (ord)
import Data.List (foldl', zipWith4, mapAccumL)
import Numeric (showIntAtBase)

alp :: String
alp = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

encode_raw :: [Int] -> String -> String -> String -> String
encode_raw input salt alpha separators = encoded' `interleave_with` seps
    where
    seps = mk_seps separators input encoded'
    encoded' = [seed] : map_accum (enc_step seedsalt) alpha input
    seedsalt = seed : salt
    seed = alpha !!% sum (zipWith rem input [100..])

mk_seps :: String -> [Int] -> [String] -> String
mk_seps seps input chunks = zipWith3 mk' [0..] input chunks
    where mk' idx val c = seps !!% (val `rem` (ord (head c) + idx))

enc_step :: String -> String -> Int -> (String, String)
enc_step salt alpha val = (alpha', last)
    where
    alpha_salt = take (length alpha) (salt ++ alpha)
    alpha' = shuffle alpha alpha_salt
    last = showIntAtBase (length alpha') (alpha' !!) val ""

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

(!!%) :: [a] -> Int -> a
xs !!% n = xs !! (n `rem` length xs)

map_accum :: (a -> b -> (a, c)) -> a -> [b] -> [c]
map_accum f accum = snd . mapAccumL f accum

interleave_with :: [[a]] -> [a] -> [a]
interleave_with [] _ = []
interleave_with chunks seps = c ++ concat (zipWith (:) seps hunks)
    where (c, hunks) = (head chunks, tail chunks)