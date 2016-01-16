-- When Sigma1 Function Has Equals Values For an Integer and Its Reversed One
-- http://www.codewars.com/kata/5619dbc22e69620e5a000010/

module Codewars.G964.Sigma1 where

import Data.List (group)
import Control.Arrow ((&&&))

equalSigma1 :: Int -> Int
equalSigma1 hmax = sum . filter eqSigma1 $ [528 .. hmax]
    where eqSigma1 n | (== show n) . reverse . show $ n = False
                     | otherwise = (== sigma1 n) . sigma1 . read . reverse . show $ n
          sigma1 1 = 1
          sigma1 n = product . map ((\(p, e) -> (p^(e+1) - 1) `div` (p - 1)) . (head &&& length)) . group . pFactors $ n
          pFactors n = facts n (2:[3,5..])
          facts n (f:fs) | n < f*f = [n]
                         | n `mod` f == 0 = f : facts (n `div` f) (f:fs)
                         | otherwise = facts n fs
