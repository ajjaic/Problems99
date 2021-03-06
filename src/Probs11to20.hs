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

type Index = Int
splitlist :: [a] -> Index -> ([a], [a])
--Given a list and an Index, split the list at the
--Index.
splitlist l n = (take n l, drop n l)

mslice :: [a] -> Index -> Index -> Maybe [a]
--Given 2 indices, return the portion of the list
--in between those indices
mslice l s e
    | (null l) || (s > e) = Nothing
    | otherwise           = (Just . take (e-s+1) . drop (s-1)) l

mrotate :: [a] -> Int -> [a]
--Rotate the list left and right
--A positive value rotates right and a negative
--value rotates left
mrotate l n
    | n > 0 = uncurry (flip (++)) (splitlist l n)
    | otherwise = uncurry (flip (++)) (splitlist l ((length l) + n))


dropkth :: [a] -> Int -> (Maybe a, [a])
--Return a tuple containing the dropped element
--and the entire list without the dropped element
--Inefficient
dropkth [] _ = (Nothing, [])
dropkth l n = (Just $ head $ drop (n-1) l, (take (n-1) l) ++ (drop n l))

mdropkth :: [a] -> Int -> (Maybe a, [a])
--Return a tuple containing the dropped element
--and the entire list without the dropped element
--More efficient than the previous version. Explicit
--recursion
mdropkth l n = fn l n []
  where
    fn [] _ ys = (Nothing, ys)
    fn (x:xs) 1 ys = (Just x, (reverse ys) ++ xs)
    fn (x:xs) i ys = fn xs (i-1) (x:ys)

