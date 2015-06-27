module Probs31to40 where

mgcd :: Int -> Int -> Int
mgcd 0 y = y
mgcd x 0 = x
mgcd x y
    | x > y     = mgcd (x-y) y
    | otherwise = mgcd x (y-x)

