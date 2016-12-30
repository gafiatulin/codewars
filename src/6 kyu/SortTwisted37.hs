-- Sorting on planet Twisted-3-7
-- https://www.codewars.com/kata/58068479c27998b11900056e

module Kata where

import Data.List (sortBy)
import Data.Function (on)

sortTwisted37 :: [Int] -> [Int]
sortTwisted37 = sortBy (compare `on` twist)
  where twist :: Int -> Int
        twist = read . map f . show
        f '3' = '7'
        f '7' = '3'
        f x = x
