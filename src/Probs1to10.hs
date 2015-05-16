
module Probs1to10 where

import Data.Maybe (catMaybes)

mlast :: [a] -> Maybe a
--Obtain last element in a list
mlast []     = Nothing
mlast [x]    = Just x
mlast (_:xs) = mlast xs

lastbutone :: [a] -> Maybe a
--Obtain last but one element in a list
lastbutone [x, _] = Just x
lastbutone (_:xs) = lastbutone xs
lastbutone _      = Nothing

kth :: [a] -> Int ->  Maybe a
--Obtain the `k`th element in a list
kth [] _     = Nothing
kth (x:_) 0  = Just x
kth (_:xs) k = kth xs (k-1)

mlen :: [a] -> Int
--Obtain the length of a list
mlen []     = 0
mlen (_:xs) = 1 + (mlen xs)

rev :: [a] -> [a]
--Reverse a list. This function is written
--in terms of `mlast` defined earlier.
--Highly inefficient. Only for demonstration.
--For better version using folds look at
--`frev`
rev = catMaybes . rev'
  where
    rev' [] = []
    rev' l  = (mlast l):(rev' (init l))

frev :: [a] -> [a]
--Reverses a list using `foldl`
frev l = foldl (\acc x -> x:acc) [] l

ispalin :: (Eq a) => [a] -> Bool
--Checks to see if a list is a palindrome or not
ispalin l = l == (frev l)


data NestedList a = Elem a
                  | List [NestedList a]
                        deriving (Show)

flatlist :: NestedList a -> [a]
--Flattens a nested list datastructure
--Makes use of list's functor and monoid instance
flatlist (Elem x) = [x]
flatlist (List l) = mconcat $ flatlist <$> l
--Perhaps this is more clearer
--flatlist (List l) = mconcat $ map flatlist l

flatten :: NestedList a -> [a]
--Flattens a nested list datastructure
--using right fold and list's monoid instance
flatten (Elem x) = [x]
flatten (List l) = foldr fn [] l
  where
    fn nl acc = mappend (flatten nl) acc

remdups :: (Eq a) => [a] -> [a]
--Remove duplicates using explicit recursion
remdups []     = []
remdups (x:xs) = x:(fn xs x)
  where
    fn [] _ = []
    fn (y:ys) y'
        | y == y'    =  fn ys y
        | otherwise  =  y:(fn ys y)

remdups' :: (Eq a) => [a] -> [a]
--Remove duplicates using a right fold
remdups' [] = []
remdups' l  = foldr fn [] l
  where
    fn y [] = [y]
    fn y ls@(y':_)
        | y == y'    =  ls
        | otherwise  =  y:ls

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack l = foldr fn [[]] l
  where
    fn x ([]:[]) = [[x]]
    fn x (n@(y':_):ys)
        | x == y' = (x:n):ys
        | otherwise = [x]:n:ys




























