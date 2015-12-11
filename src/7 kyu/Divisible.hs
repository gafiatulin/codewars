-- Is n divisible by (...)?
-- http://www.codewars.com/kata/558ee8415872565824000007

module Codewars.Kata.Divisible where

isDivisible :: Integral n => n -> [n] -> Bool
isDivisible n xs = all (\x -> n `mod` x == 0) xs
