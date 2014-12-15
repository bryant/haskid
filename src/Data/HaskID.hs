{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.HaskID where

import Data.Char (ord)
import Data.List (foldl', zipWith4, mapAccumL, (\\), elemIndex, nub, intersect)
import Data.Maybe (fromJust)
import Numeric (showIntAtBase, readInt)

alp :: String
alp = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

data CfgTag = ValidConfig | InvalidConfig

data Config (proxy :: CfgTag)
    = Config
    { alphabet :: String
    , separators :: String
    , min_hash_length :: Int
    , salt :: String
    , guards :: String
    }
    deriving Show

min_alphabet_length = 16
separator_ratio = 3.5
guard_ratio = 12
default_separators = "cfhistuCFHISTU"

init_config :: String -> String -> Int -> Either String (Config ValidConfig)
init_config salt alpha minlen
    | length (nub alpha) < 16 = Left "alphabet must be 16+ unique characters."
    | otherwise = Right $ Config alpha' seps (max 0 minlen) salt guards
    where
    (alpha', seps, guards) = uncurry process2 $ process1 alpha

    process1 as
        | some > 0 = (drop some alf, sep' ++ take some alf)
        | otherwise = (alf, sep')
        where
        sep' = default_separators `intersect` as
        alf = shuffle (nub as \\ default_separators) salt
        some = ceiling (fromIntegral (length alf) / separator_ratio) - length sep'

    process2 as ss
        | length as < 3 = (as, drop some ss, take some ss)
        | otherwise = (drop some as, ss, take some as)
        where some = length as `ceildiv` guard_ratio

default_settings :: Config ValidConfig
default_settings = Config
    { alphabet = drop len_guards alpha''
    , separators = sep'
    , min_hash_length = 0
    , salt = salt'
    , guards = take len_guards alpha''
    }
    where
    salt' = ""
    alpha' = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
    sep' = shuffle "cfhistuCFHISTU" salt'
    alpha'' = shuffle (alpha' \\ sep') salt'
    len_guards = length alpha'' `ceildiv` guard_ratio

ceildiv :: Integral n => n -> n -> n
ceildiv i j = (i + j - 1) `quot` j

encode :: Config ValidConfig -> [Int] -> String
encode (Config alpha separators min_length salt guards) input
    | long_enough raw = raw
    | long_enough graw = graw
    | long_enough grawg = grawg
    | otherwise = shuffle_pad alpha grawg min_length
    where
    raw = encode_raw input salt alpha separators
    graw = guard_choice (head raw) : raw
    grawg = graw ++ [guard_choice (raw !! 1)]
    long_enough = (>= min_length) . length
    guard_choice n = guards !!% (sum (zipWith rem input [100..]) + ord n)

decode :: Config ValidConfig -> String -> [Int]
decode (Config alpha separators min_length salt guards) encoded =
    case str_split (`elem` guards) encoded of
        [_, seed : it, _] -> decode' it $ seed : salt
        [_, seed : it] -> decode' it $ seed : salt
        [seed : it] -> decode' it $ seed : salt
        _ -> []
    where decode' it s = map_accum (dec_step s) alpha $ str_split (`elem` separators) it

dec_step :: String -> String -> String -> (String, Int)
dec_step salt alpha radixed = (alpha', int)
    where
    alpha_salt = take (length alpha) (salt ++ alpha)
    alpha' = shuffle alpha alpha_salt
    int = fst . head $ readInt (length alpha') (`elem` alpha') index_in radixed
    index_in = fromJust . (`elemIndex` alpha')

shuffle_pad alpha xs min_length
    | length xs >= min_length = take min_length $ drop exlen2 xs
    | otherwise = shuffle_pad alpha' xs' min_length
    where
    alpha' = shuffle alpha alpha
    xs' = take d alpha' ++ xs ++ drop d alpha' where d = length alpha `quot` 2
    exlen2 = (length xs - min_length) `quot` 2

encode_raw :: [Int] -> String -> String -> String -> String
encode_raw input salt alpha separators = init $ seed : interleave_with encoded' seps
    where
    seps = mk_seps separators input encoded'
    encoded' = map_accum (enc_step seedsalt) alpha input
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
mk_swap_points start [] = []
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
interleave_with chunks seps = concat (zipWith (++) chunks seps')
    where seps' = map (: []) seps

str_split :: Eq a => (a -> Bool) -> [a] -> [[a]]
str_split f = filter (/= []) . foldr f' [[]]
    where f' c accum
            | f c = [] : accum
            | otherwise = (c : head accum) : tail accum
