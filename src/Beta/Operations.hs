-- Operations with sequence
-- https://www.codewars.com/kata/596ddaccdd42c1cf0e00005c

module Operations where

import Data.List (zipWith4)

calc :: [Int] -> Int
calc = sum . zipWith4 (\f1 f2 f3 e -> f1 . f2 . f3 $ e) (cycle [id, id, id, id, negate]) (cycle [id, id, (*3)]) (repeat f)
    where f x | x > 0 = x^2
              | otherwise = x
