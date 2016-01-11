-- Transportation on vacation
-- http://www.codewars.com/kata/568d0dd208ee69389d000016/

module Codewars.G964.Rentalcarcost where

rentalCarCost :: Int -> Int
rentalCarCost d | d < 3 = d * 40
                | d < 7 = d * 40 - 20
                | otherwise = d * 40 - 50
