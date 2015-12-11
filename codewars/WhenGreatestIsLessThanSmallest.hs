-- When greatest is less than smallest
-- http://www.codewars.com/kata/55f2a1c2cb3c95af75000045/

module Kata.WhenGreatestIsLessThanSmallest where

greatest :: Integer -> Integer -> Integer -> Integer
greatest x y n = ((n - 1) `div` (lcm x y)) * lcm x y
smallest :: Integer -> Integer -> Integer -> Integer
smallest x y n = ((n `div` (lcm x y)) + 1) * lcm x y
