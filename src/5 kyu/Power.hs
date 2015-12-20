-- By the Power Set of Castle Grayskull
-- http://www.codewars.com/kata/53d3173cf4eb7605c10001a8/

module Power where

power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = map (x:) (power xs) ++ power xs
