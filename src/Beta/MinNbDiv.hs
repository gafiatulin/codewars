-- Find the First Number in Having a Certain Number of Divisors I
-- http://www.codewars.com/kata/5612ab201830eb000f0000c0/

module Codewars.G964.MinNbDiv where

import Data.List (find, group)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

findMinNum m = fromJust . find ((==m) . tau) $ [1..]
    where tau 1 = 1
          tau n = product . map (succ . length) . group . pFactors $ n
          pFactors n = facts n (2:[3,5..])
          facts n (f:fs) | n < f*f = [n]
                         | n `mod` f == 0 = f : facts (n `div` f) (f:fs)
                         | otherwise = facts n fs
