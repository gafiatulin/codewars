-- Watermelon
-- http://www.codewars.com/kata/55192f4ecd82ff826900089e

module Codewars.Kata.Watermelon where

divide :: Integer -> Bool
divide w | w <= 2 = False
         | otherwise = even w
