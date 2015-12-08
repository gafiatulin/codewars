-- Convert number to reversed array of digits
-- http://www.codewars.com/kata/5583090cbe83f4fd8c000051

module Codewars.Kata.Convert where

digitize :: Int -> [Int]
digitize s | s `div` 10 == 0 = [s `mod` 10]
           | otherwise = s `mod` 10 : digitize (s `div` 10)
