-- Find the discounted prices
-- http://www.codewars.com/kata/56f3ed90de254a2ca7000e20

module Codewars.Kata.Discount where

import Data.List (delete)

findDiscounted = unwords . map (show . round) . f . map read . words
    where f [] = []
          f (x:xs) | (4*x/3) `elem` xs = (x :) . f . delete (4*x/3) $ xs
                   | otherwise = f xs
