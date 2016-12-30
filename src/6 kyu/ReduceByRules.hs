-- Reducing by rules to get the result
-- https://www.codewars.com/kata/585ba6dff59b3cef3f000132

module Kata (reduceByRules) where

reduceByRules :: [Double] -> [Double -> Double -> Double] -> Double
reduceByRules [] _ = 0
reduceByRules _ [] = 0
reduceByRules (n: ns) fs = g n ns (cycle fs)
    where g a [] _ = a
          g a (x : xs) (f:fs) = g (f a x) xs fs
