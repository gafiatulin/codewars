-- Moduli number system
-- http://www.codewars.com/kata/54db15b003e88a6a480000b9/

module Codewars.Kata.ModSys where

import Data.List (intercalate, tails)

fromNb2Str :: Integer -> [Integer] -> String
fromNb2Str n xs | n > product xs = "Not applicable"
                | or . zipWith (\x px -> any (\p -> gcd x p > 1) px) xs $ (tail . tails $ xs) = "Not applicable"
                | otherwise = "-" ++ (intercalate "--" . map (show . (n `mod`)) $ xs) ++ "-"
