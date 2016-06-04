-- Some Egyptian fractions
-- http://www.codewars.com/kata/54f8693ea58bce689100065f/

module Codewars.Kata.Decompose where

import Data.List (unfoldr)
import Data.Ratio ((%), numerator, denominator)

decompose :: String -> String -> [String]
decompose n d | i /= 0 = show i : decompose (show . numerator $ f) (show . denominator $ f)
              | otherwise = map (("1/"++) . show) . unfoldr g $ f
              where (i, f) = properFraction $ read n % read d
                    g 0 = Nothing
                    g x = Just (ceiling (recip x), x - 1 % ceiling (recip x))
