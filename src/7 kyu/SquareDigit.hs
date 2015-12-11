-- Square Every Digit
-- http://www.codewars.com/kata/546e2562b03326a88e000020

module SquareDigit where

import Data.Char

squareDigit :: Int -> Int
squareDigit n = (sign*) . read . f . filter isNumber . show $ n 
    where sign = if n < 0 then -1 else 1
          f    = concatMap (\x -> show . (^2) $ (read [x] :: Int))
