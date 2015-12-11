-- Number Pairs
-- http://www.codewars.com/kata/563b1f55a5f2079dc100008a/

module Codewars.LargeNumberGetter where

getLargerNumbers :: Ord a => [a] -> [a] -> [a]
getLargerNumbers = zipWith max
