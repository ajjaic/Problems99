module Probs21to30 where

import System.Random
import Data.List (sortBy, groupBy)

insertat :: [a] -> a -> Int -> [a]
--Inserts an element at a specific index in
--the list
insertat [] e _ = [e]
insertat (x:xs) e 1 = e:x:xs
insertat (x:xs) e n = x:(insertat xs e (n-1))

mrange :: Int -> Int -> [Int]
--Explicit implementation of the range function
mrange s e
    | s == e = [s]
    | otherwise = s:(mrange (s+1) e)

seed :: StdGen
--A sample seed used for some of the subsequent
--exercises
seed = mkStdGen 103

rndels :: [a] -> Int -> [a]
--Randomly choose a given number of elements
rndels xs n = take n [xs !! i | i <- randomRs (0, length xs-1) seed]

distinctrnd :: Int -> (Int, Int) -> [Int]
--Randomly choose a given number of elements,
--ensuring that no element is chosen more than
--once
distinctrnd n (s, e) = take n $ fn [s..e]
  where
    fn xs = maybe [] (\(y, ys) -> y:(fn ys)) (helper xs)

helper :: [a] -> Maybe (a, [a])
helper [] = Nothing
helper xs = Just (xs !! fstel, sndel)
  where
    fstel = fst $ randomR (0, lenl-1) seed
    sndel = (take fstel xs) ++ (drop (fstel+1) xs)
    lenl  = length xs

isLeapYear :: Int -> Bool
--Determine if a year is leap or not
isLeapYear y = (isdiv 400) || ((isdiv 4) && (not $ isdiv 100))
  where
    isdiv x = (y `mod` x) == 0

perms :: [a] -> [[a]]
perms [] = []
perms [x] = [[x]]
perms l@(x:xs) = concat $ map (fn p) [1 ..(length l)]
  where
    {-fn :: [[a]] -> Int -> [[a]]-}
    p = perms xs
    fn ys n = map (\y -> insertat y x n) ys



sortByListLen :: [[a]] -> [[a]]
--Given a list of lists, sort the list based on the length of the
--sublists. Smaller lists in the front with the longer lists at the
--end.
sortByListLen = sortBy (\a b -> compare (length a) (length b))


sortByFrequency :: [[a]] -> [[a]]
--Given a list of lists, sort them according to the frequency of
--their lengths
sortByFrequency ls = concat
                    $ sortByListLen
                    $ groupBy fn
                    $ sortByListLen ls
    where
        fn a b = (length a) == (length b)









