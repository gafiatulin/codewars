-- 8 towers
-- http://www.codewars.com/kata/535bea76cdbf50281a00004c/
module Towers where

towerCombination :: Integer -> Integer
towerCombination 0 = 1
towerCombination n = n * towerCombination (n - 1)
