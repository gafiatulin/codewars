-- Boiled Eggs
-- http://www.codewars.com/kata/52b5247074ea613a09000164/

module BoiledEggs where

cookingTime :: Integer -> Integer
cookingTime 0 = 0
cookingTime n = 5 * (1 + (n-1) `div` 8)
