-- Find the smallest
-- https://www.codewars.com/kata/573992c724fc289553000e95

module Codewars.G964.Tosmallest where

smallest :: Integer -> (Integer, Int, Int)
smallest n = minimum [ (read . f i j . show $ n , i, j) | i <- is, j <- is]
    where is = [0 .. pred . length . show $ n]
          f i j xs = let (e, l) = g i xs in h j e l
          g i xs = (xs !! i, take i xs ++ drop (i + 1) xs)
          h i e xs = take i xs ++ [e] ++ drop i xs
