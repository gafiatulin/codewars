-- Heads and Legs
-- http://www.codewars.com/kata/574c5075d27783851800169e

module Animals where

animals :: Int -> Int -> Maybe (Int,Int)
animals h l | h < 0 || l < 0 = Nothing
            | otherwise = if g (l-2*h) || g (4*h-l) then Nothing else Just ((4*h-l) `div` 2, (l-2*h) `div` 2)
            where g x =  odd x || x < 0
