-- Total increasing or decreasing numbers up to a power of 10
-- https://www.codewars.com/kata/55b195a69a6cc409ba000053

module Kata.TotalIncreasingOrDecreasingNumbers where

totalIncDec :: Integer -> Integer
totalIncDec 0 = 1
totalIncDec 1 = 10
totalIncDec n = (2 * f n) + (sum . map f $ [2..n-1]) - 10 * (n-1)
    where f x = g x [10..] `div` g x [1..]
          g x = product . take (fromIntegral x)
