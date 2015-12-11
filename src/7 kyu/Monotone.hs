-- Monotone travel
-- http://www.codewars.com/kata/54466996990c921f90000d61

module Monotone where

isMonotone :: Ord a => [a] -> Bool
isMonotone x = and $ zipWith (<=) x (tail x)
