-- A Rule of Divisibility by 13
-- http://www.codewars.com/kata/564057bc348c7200bd0000ff

module Codewars.G964.Thirteen where

thirt n | n == g n = n
        | otherwise = thirt (g n)
        where f x | (x `div` 10) == 0 = [x `mod` 10]
                  | otherwise = (x `mod` 10):f (x `div` 10)
              g x = sum (zipWith (*) (cycle [1, 10, 9, 12, 3, 4])  (f x))
