{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.HaskID
    ( encode
    , decode
    , init_config
    , default_settings
    ) where

import qualified Data.Array.Unboxed as Array
import qualified Data.ByteString as B

import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed (UArray, bounds)
import Data.Array.ST (thaw, freeze, STUArray)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeIndex, unsafeHead)
import Data.Char (ord)
import Data.List (foldl', mapAccumL, (\\), elemIndex, nub, intersect)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Numeric (showIntAtBase, readInt)

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
        sep' = shuffle (default_separators `intersect` as) salt
        alf = shuffle (nub as \\ default_separators) salt
        some = ceiling (fromIntegral (length alf) / separator_ratio) - length sep'

    process2 as ss
        | length as < 3 = (as, drop some ss, take some ss)
        | otherwise = (drop some as, ss, take some as)
        where some = length as `ceildiv` guard_ratio

default_settings :: Config ValidConfig
Right default_settings = init_config "" alpha' 0
    where
    alpha' = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

ceildiv :: Integral n => n -> n -> n
ceildiv i j = (i + j - 1) `quot` j

encode :: Config ValidConfig -> [Int] -> String
encode (Config alpha separators min_length salt guards) input
    | not $ all (>= 0) input = ""
    | long_enough raw = raw
    | long_enough graw = graw
    | long_enough grawg = grawg
    | otherwise = shuffle_pad alpha' grawg min_length
    where
    (alpha', raw) = encode_raw input salt alpha separators
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
    where decode' it s = snd $ mapAccumL (dec_step s) alpha $
                                str_split (`elem` separators) it

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
    xs' = drop d alpha' ++ xs ++ take d alpha' where d = length alpha `quot` 2
    exlen2 = (length xs - min_length) `quot` 2

encode_raw :: [Int] -> String -> String -> String -> (String, String)
encode_raw input salt alpha separators =
    (alpha', init $ seed : interleave_with encoded' seps)
    where
    seps = mk_seps separators input encoded'
    (alpha', encoded') = mapAccumL (enc_step seedsalt) alpha input
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

salted_shuffle :: UArray Int Word8 -> ByteString -> UArray Int Word8
salted_shuffle input salt = runST $
    thaw input >>= loop (arrlen input - 1) (fromIntegral $ unsafeHead salt) 0
               >>= freeze
    where
    loop :: Int -> Int -> Int -> (STUArray s) Int Word8
         -> ST s ((STUArray s) Int Word8)
    loop !ind !summ !grainpos arr = if ind < 1 then return arr else swap
        where
        swap = do
            k <- unsafeRead arr ind
            unsafeRead arr alt >>= unsafeWrite arr ind >> unsafeWrite arr alt k
            loop (ind - 1) summ' grainpos' arr
        alt = (summ + grainpos + grain) `rem` ind
        grain = fromIntegral $ unsafeIndex salt grainpos
        grainpos' = (grainpos + 1) `rem` B.length salt
        summ' = (summ + fromIntegral grain')
            where grain' = unsafeIndex salt grainpos'

arrlen = snd . bounds

shuffle :: String -> String -> String
shuffle xs = Array.elems . foldl' swap xs' . mk_swap_points (length xs)
    where xs' = Array.listArray (0, length xs - 1) xs

mk_swap_points :: Int -> String -> [(Int, Int)]
mk_swap_points start [] = []
mk_swap_points start salt =
    zipWith3 compute sumsalt [start - 1, start - 2..1] salts
    where
    salts = cycle . zip [0..] $ map ord salt
    sumsalt = scanl1 (+) $ map snd salts
    compute p i (v, integer) = (i, (integer + v + p) `rem` i)

swap :: Array.Array Int Char -> (Int, Int) -> Array.Array Int Char
swap as (i, j) = as Array.// [(i, as Array.! j), (j, as Array.! i)]

(!!%) :: [a] -> Int -> a
xs !!% n = xs !! (n `rem` length xs)

interleave_with :: [[a]] -> [a] -> [a]
interleave_with chunks seps = concat (zipWith (++) chunks seps')
    where seps' = map (: []) seps

str_split :: Eq a => (a -> Bool) -> [a] -> [[a]]
str_split f = filter (/= []) . foldr f' [[]]
    where f' c accum
            | f c = [] : accum
            | otherwise = (c : head accum) : tail accum
