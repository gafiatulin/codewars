-- sum2total
-- http://www.codewars.com/kata/559fed8454b12433ff0000a2/

module Codewars.Kata.Sum2Total where

total :: Num a => [a] -> a
total [] = 0
total [x] = x
total xs = total $ zipWith (+) xs (tail xs)
