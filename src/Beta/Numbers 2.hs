-- Perfect Number Verifier
-- http://www.codewars.com/kata/56a28c30d7eb6acef700004d/

module Codewars.Numbers where

import Data.List (group)
import Control.Arrow((&&&))

isPerfect :: Int -> Bool
isPerfect = uncurry (==) . (id &&& sigma1')
    where sigma1' 1 = 0
          sigma1' n = (+ negate n) . product . map ((\(p, e) -> (p^(e+1) - 1) `div` (p - 1)) . (head &&& length)) . group . pFactors $ n
          pFactors n = facts n (2:[3,5..])
          facts n (f:fs) | n < f*f = [n]
                         | n `mod` f == 0 = f : facts (n `div` f) (f:fs)
                         | otherwise = facts n fs
