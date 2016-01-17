-- Raise Me to The Third Power, Search My Divisors... .....Could You Believe that?
-- http://www.codewars.com/kata/56060ba7b02b967eb1000013/

module Codewars.G964.Thirdpow where

import Data.List (group)
import Control.Arrow ((&&&))

intCubeSumDiv :: Int -> Integer
intCubeSumDiv = (filter hasProperty [1 ..] !!)
    where hasProperty x = x^3 `mod` (sigma1 x) == 0
          sigma1 1 = 1
          sigma1 n = product . map ((\(p, e) -> (p^(e+1) - 1) `div` (p - 1)) . (head &&& length)) . group . pFactors $ n
          pFactors n = facts n (2:[3,5..])
          facts n (f:fs) | n < f*f = [n]
                         | n `mod` f == 0 = f : facts (n `div` f) (f:fs)
                         | otherwise = facts n fs
