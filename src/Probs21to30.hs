module Probs21to30 where

insertat :: [a] -> a -> Int -> [a]
insertat [] _ _ = []
insertat (x:xs) e 1 = e:x:xs
insertat (x:xs) e n = x:(insertat xs e (n-1))
