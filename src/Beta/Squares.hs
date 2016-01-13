-- Squares sequence
-- http://www.codewars.com/kata/5546180ca783b6d2d5000062/

module Codewars.Exercise.Squares where

squares :: Integer -> Int -> [Integer]
squares x n = take n . iterate (^2) $ x
