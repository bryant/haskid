{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Data.HaskID
    ( encode
    , decode
    , init_config
    , default_settings
    ) where

import qualified Data.Vector.Unboxed as Vec

import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (swap)

import Control.Monad.ST (ST, runST)
import Data.Char (ord, chr)
import Data.List (foldl', mapAccumL, (\\), elemIndex, nub, intersect)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Numeric (showIntAtBase, readInt)

data CfgTag = ValidConfig | InvalidConfig

data Config (proxy :: CfgTag)
    = Config
    { alphabet :: Vector Int
    , separators :: Vector Int
    , min_hash_length :: Int
    , salt :: Vector Int
    , guards :: Vector Int
    }
    deriving Show

min_alphabet_length = 16 :: Int
separator_ratio = 3.5 :: Double
guard_ratio = 12 :: Double
default_separators = map ord "cfhistuCFHISTU"

init_config :: String -> String -> Int -> Either String (Config ValidConfig)
init_config salt alpha minlen
    | length (nub alpha) < 16 = Left "alphabet must be 16+ unique characters."
    | otherwise = Right $ Config as ss (max 0 minlen) salt' gs
    where
    (as, ss, gs) = uncurry process2 . process1 $ map ord alpha
    salt' = Vec.fromList $ map ord salt

    process1 as
        | some > 0 = (Vec.drop some alf, sep' Vec.++ Vec.take some alf)
        | otherwise = (alf, sep')
        where
        sep' = shuffle (Vec.fromList $ default_separators `intersect` as) salt'
        alf = shuffle (Vec.fromList $ nub as \\ default_separators) salt'
        some = Vec.length alf `ceildiv` separator_ratio - Vec.length sep'

    process2 as ss
        | Vec.length as < 3 = (as, Vec.drop some ss, Vec.take some ss)
        | otherwise = (Vec.drop some as, ss, Vec.take some as)
        where some = Vec.length as `ceildiv` guard_ratio

    ceildiv a b = ceiling $ fromIntegral a / b

default_settings :: Config ValidConfig
Right default_settings = init_config "" alpha' 0
    where
    alpha' = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

encode :: Config ValidConfig -> [Int] -> String
encode (Config alpha separators min_length salt guards) input
    | not $ all (>= 0) input = ""
    | long_enough raw = map chr raw
    | long_enough graw = map chr graw
    | long_enough grawg = map chr grawg
    | otherwise = map chr $ shuffle_pad alpha' grawg min_length
    where
    (alpha', raw) = encode_raw input salt alpha separators
    graw = guard_choice (head raw) : raw
    grawg = graw ++ [guard_choice (raw !! 1)]
    long_enough = (>= min_length) . length
    guard_choice n = guards !~% (sum (zipWith rem input [100..]) + n)

decode :: Config ValidConfig -> String -> [Int]
decode (Config alpha separators min_length salt guards) encoded =
    case str_split (`Vec.elem` guards) $ map ord encoded of
        [_, seed : it, _] -> decode' it $ Vec.cons seed salt
        [_, seed : it] -> decode' it $ Vec.cons seed salt
        [seed : it] -> decode' it $ Vec.cons seed salt
        _ -> []
    where decode' it s = snd $ mapAccumL (dec_step s) alpha $
                                str_split (`Vec.elem` separators) it

dec_step :: Vector Int -> Vector Int -> [Int] -> (Vector Int, Int)
dec_step salt alpha radixed = (alpha', int)
    where
    alpha_salt = Vec.take (Vec.length alpha) (salt Vec.++ alpha)
    alpha' = shuffle alpha alpha_salt
    int = read_base (Vec.length alpha') index_in radixed
    index_in = fromJust . (`Vec.elemIndex` alpha')

read_base :: Integral a => a -> (b -> a) -> [b] -> a
read_base base f (x:xs)
    | base < 1 = error "base < 1"
    | otherwise = go xs $ f x
    where
    go [] n = n
    go (x:xs) n = go xs $ f x + n * base

shuffle_pad :: Vector Int -> [Int] -> Int -> [Int]
shuffle_pad alpha xs min_length
    | length xs >= min_length = take min_length $ drop exlen2 xs
    | otherwise = shuffle_pad alpha' xs' min_length
    where
    alpha' = shuffle alpha alpha
    xs' = Vec.toList (Vec.drop d alpha') ++ xs ++ Vec.toList (Vec.take d alpha')
        where d = Vec.length alpha' `quot` 2
    exlen2 = (length xs - min_length) `quot` 2

encode_raw :: [Int] -> Vector Int -> Vector Int -> Vector Int
           -> (Vector Int, [Int])
encode_raw input salt alpha separators =
    (alpha', init $ seed : interleave_with encoded seps)
    where
    seps = mk_seps separators input encoded
    (alpha', encoded) = mapAccumL (enc_step seedsalt) alpha input
    seedsalt = Vec.cons seed salt
    seed = alpha !~% sum (zipWith rem input [100..])

mk_seps :: Vector Int -> [Int] -> [[Int]] -> [Int]
mk_seps seps input chunks = zipWith3 mk' [0..] input $ map head chunks
    where mk' idx val c = seps !~% (val `rem` (c + idx))

enc_step :: Vector Int -> Vector Int -> Int -> (Vector Int, [Int])
enc_step salt alpha val = (alpha', last)
    where
    alpha' = shuffle alpha $ Vec.take (Vec.length alpha) (salt Vec.++ alpha)
    last = to_base (Vec.length alpha) (alpha' !~) val

to_base :: Integral a => a -> (a -> b) -> a -> [b]
to_base base f n
    | base <= 1 = error "base <= 1"
    | n < 0 = error "negative"
    | otherwise = go (n `quot` base) [f $ n `rem` base]
    where
    go 0 accum = accum
    go n accum = go q $ f r : accum where (q, r) = quotRem n base

shuffle :: Vector Int -> Vector Int -> Vector Int
shuffle input salt | Vec.length salt == 0 = input | otherwise = runST $
    Vec.thaw input >>= loop (Vec.length input - 1) (salt !~ 0) 0 >>= Vec.freeze
    where
    loop !ind !summ !grainpos vec
        | ind < 1 = return vec
        | otherwise = swap vec ind alt >> loop (ind - 1) summ' grainpos' vec
        where
        alt = (summ + grainpos + salt !~ grainpos) `rem` ind
        grainpos' = (grainpos + 1) `rem` Vec.length salt
        summ' = (summ + salt !~ grainpos')

(!~) :: Vec.Unbox a => Vector a -> Int -> a
(!~) = Vec.unsafeIndex

(!~%) :: Vec.Unbox a => Vector a -> Int -> a
vec !~% n = Vec.unsafeIndex vec $ n `rem` Vec.length vec

interleave_with :: [[a]] -> [a] -> [a]
interleave_with chunks seps = concat (zipWith (++) chunks seps')
    where seps' = map (: []) seps

str_split :: Eq a => (a -> Bool) -> [a] -> [[a]]
str_split f = filter (/= []) . foldr f' [[]]
    where f' c accum
            | f c = [] : accum
            | otherwise = (c : head accum) : tail accum
