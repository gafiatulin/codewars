-- Hamming Distance - Part 1: Binary codes
-- http://www.codewars.com/kata/5624e574ec6034c3a20000e6/

module Codewars.HammingDistance where

hammingDistance :: String -> String -> Int
hammingDistance = curry ( length . filter not . uncurry (zipWith (==)) )
