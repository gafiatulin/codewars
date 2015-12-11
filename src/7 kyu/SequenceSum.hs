-- SequenceSum
-- http://www.codewars.com/kata/5436f26c4e3d6c40e5000282

module SequenceSum where

import Data.List (inits)

sumOfN :: Int -> [Int]
sumOfN n | n < 0 = f [0, -1 .. n]
         | otherwise = f [0 .. n]
         where f = (map sum) . tail . inits
