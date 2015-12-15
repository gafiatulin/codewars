-- Permutations and Dot Products
-- http://www.codewars.com/kata/5457ea88aed18536fc000a2c/

module MinimumDot where

import Data.List (sort)

minDot :: (Ord a, Num a) => [a] -> [a] -> a
minDot xs ys = dot (sort xs) (reverse . sort $ ys)
    where dot x y = sum $ zipWith (*) x y
