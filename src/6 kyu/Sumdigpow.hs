-- Take a Number And Sum Its Digits Raised To The Consecutive Powers And ....¡Eureka!!
-- http://www.codewars.com/kata/5626b561280a42ecc50000d1

module Codewars.G964.Sumdigpow where

import Data.Char(digitToInt)

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter f [a..b]
  where f n = (== n) . sum . zipWith (flip (^)) [1..] . map digitToInt . show $ n