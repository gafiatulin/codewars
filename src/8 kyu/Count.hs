-- Count by X
-- http://www.codewars.com/kata/5513795bd3fafb56c200049e

module Codewars.Kata.Count where

countBy :: Integer -> Int -> [Integer]
countBy x n = take n [x, x+x ..]
