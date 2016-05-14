-- From-To-Step Sequence Generator
-- http://www.codewars.com/kata/56459c0df289d97bd7000083

module FromToStepGenerator where

generator :: Integer -> Integer -> Integer -> [Integer]
generator n m 0 = []
generator n m k | n <= m = [n, n + k .. m]
                | otherwise = [n, n - k .. m]
