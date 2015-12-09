-- Breaking chocolate problem
-- http://www.codewars.com/kata/534ea96ebb17181947000ada/
-- Note that one of the submission tests is incorrect. To pass use /= 0 instead of > 0.

module BreakingChocolate where

breakChocolate :: Int -> Int -> Int
breakChocolate n m | n > 0 && m > 0 = (n-1) + (m-1) * n
                   | otherwise = 0
