-- Fibo akin
-- https://www.codewars.com/kata/5772382d509c65de7e000982

module Codewars.G964.Fibkind(comp, lengthSupUK) where

s = 1 : 1 : zipWith3 f s (tail s) [2..]
    where f a b i = s !! (i - a) + s !! (i - b)

lengthSupUK :: Int -> Int -> Int
lengthSupUK n k = length . filter (>= k) . take n $ s

comp :: Int -> Int
comp n  = length . filter (<0) . take (n - 1) . zipWith (-) (tail s) $ s
