-- Pizza pieces
-- http://www.codewars.com/kata/5551dc71101b2cf599000023/

module Codewars.Kata.Pizza where

maxPizza :: Integer -> Maybe Integer
maxPizza n | n >= 0 = Just ((n^2 + n + 2) `div` 2)
           | otherwise = Nothing
