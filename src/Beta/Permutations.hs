-- Permutations
-- https://www.codewars.com/kata/5993eec34f5d9f2504000051

module Kata.Permutations (permutations) where

import qualified Data.List as L (transpose, permutations)

permutations :: [String] -> Int
permutations = minimum . map (f . map read . L.transpose) . L.permutations . L.transpose
    where f [] = 0
          f (x:xs) = uncurry (-) . foldr (\e (l, s) -> (max e l, min e s)) (x, x) $ xs
