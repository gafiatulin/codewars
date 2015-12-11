-- A Rule of Divisibility by 7
-- http://www.codewars.com/kata/55e6f5e58f7817808e00002e

module Codewars.G964.Seven where

import Control.Arrow (second)

seven :: Integer -> (Integer, Int)
seven m | m == 0 = (0, 0)
        | (length . show . step $ m) <= 2 = (step m, 1)
        | otherwise = second (1 +) (seven . step $ m) 
        where step m = (m `div` 10)  - 2 * (m `mod` 10)
