-- How many twos?
-- http://www.codewars.com/kata/56aed5db9d5cb55de000001c/

module Codewars.Numbers where

twoCount :: Int -> Int
twoCount n | m == 0 = succ . twoCount $ d
           | otherwise = 0
           where (d, m) = n `divMod` 2
