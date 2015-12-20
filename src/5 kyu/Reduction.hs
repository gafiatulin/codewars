-- Directions Reduction
-- http://www.codewars.com/kata/550f22f4d758534c1100025a/

module Codewars.Kata.Reduction where
import Codewars.Kata.Reduction.Direction

-- data Direction = North | East | West | South deriving (Eq)

dirReduce :: [Direction] -> [Direction]
dirReduce = foldr reduce []
    where reduce North (South:xs) = xs
          reduce South (North:xs) = xs
          reduce East (West:xs) = xs
          reduce West (East:xs) = xs
          reduce x xs = x:xs
