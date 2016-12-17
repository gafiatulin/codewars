-- Double Every Other
-- https://www.codewars.com/kata/5809c661f15835266900010a

module Codewars.Kata.DoubleEveryOther where

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [id,(*2)])
