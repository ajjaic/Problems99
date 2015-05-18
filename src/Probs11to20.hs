module Probs11to20 where

import Probs1to10 (encode, pack)


data CharEncode a = Multiple Int a
                  | Single a
                    deriving (Show)

encodemodified :: (Eq a) => [a] -> [CharEncode a]
--Given a list, encodes the list using a custom
--type
encodemodified = (map helper) . encode . pack
  where
    helper (1, c) = Single c
    helper (n, c) = Multiple n c


decodemodified :: [CharEncode a] -> [a]
--Given the custom type, decodes the encoded
--value to the original list
decodemodified l = mconcat $ map fn l
  where
    fn (Single x)     = [x]
    fn (Multiple n x) = replicate n x

directencode :: (Eq a) => [a] -> [CharEncode a]
--This is the same as problem 9
directencode = encodemodified

mdup :: [a] -> [a]
--Duplicates the elements in the list
mdup []     = []
mdup (x:xs) = x:x:(mdup xs)

mrepli :: [a] -> Int -> [a]
--Replicates the elements by the given count
mrepli l n = fn l n
  where
    fn [] _       = []
    fn (_:xs) 0   = fn xs n
    fn l'@(x:_) m = x:(fn l' (m-1))

dropnth :: [a] -> Int ->  [a]
--Drop every nth element in the list
dropnth l n = map fst
            $ filter (not . (==n) . snd)
            $ zip l (mconcat $ repeat [1..n])



