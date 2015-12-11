-- Alphabetize a list by the nth character
-- http://www.codewars.com/kata/54eea36b7f914221eb000e2f

module Codewars.Exercise.AlphaN where

import Data.List (sortBy)
import Data.Ord (comparing)

sortIt :: [String] -> Int -> [String]
sortIt [] _ = []
sortIt ss n = sortBy (cmp n) ss 
    where cmp n xs ys = f (compare (xs!!(n-1)) (ys!!(n-1)))
        where f EQ = compare xs ys
              f  a = a

