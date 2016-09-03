-- Lagrange Interpolation
-- https://www.codewars.com/kata/52d4862dae18a44bf500016b

module Codewars.Jacobb.Lagrange where

import Data.List (inits, tails)

lagrange :: (Eq a, Fractional a) => [(a, a)] -> a -> a
lagrange points x = sum . zipWith (*) ys . map (\(a, b) -> product . map (\c -> (x - c) / (a - c) ) $ b ) . zip xs . zipWith (++) (inits xs) . tail . tails $ xs
    where (xs, ys) = unzip points
