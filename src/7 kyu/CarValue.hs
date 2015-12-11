-- Car Depreciation
-- http://www.codewars.com/kata/55521d28f790f6d92d0001ab

module Codewars.Kata.CarValue where

import Text.Printf (printf)

car :: Float -> Integer -> String
car p n = printf "%.2f" (price p n :: Float) 
    where price p 0 = p
          price p 1 = 0.8  * price p 0
          price p 2 = 0.64 * price p 0
          price p n = 0.9  * price p (n - 1)
