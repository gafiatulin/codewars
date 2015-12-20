module Codewars.G964.SumOfK where

import Data.List (sortBy)
import Data.Maybe (listToMaybe)

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum t n = listToMaybe . sortBy (flip compare) . filter (<=t) . map sum . combinations n
    where combinations 0 _ = [[]]
          combinations _ [] = []
          combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs