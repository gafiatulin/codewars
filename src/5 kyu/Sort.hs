-- Custom sort function
-- http://www.codewars.com/kata/52105fab0bd0ce9dd00000fe/
-- Note: Tests fails for sort [] because of ambiguous typing.

module Codewars.Kata.Sort where

sort :: Ord a => [a] -> [a]
sort []  = []
sort [x] = [x]
sort xs = merge (sort f) (sort s)
    where (f, s) = split xs
          merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
                              | otherwise = y:merge (x:xs) ys
          split []       = ([],[])
          split [x]      = ([x],[])
          split (x:y:z) = (x:xs,y:ys)
              where (xs,ys) = split z
