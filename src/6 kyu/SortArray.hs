-- Sort the odd
-- https://www.codewars.com/kata/578aa45ee9fd15ff4600090d

module SortArray where

import Data.List(partition, sort, sortBy)
import Control.Arrow ((***), first)
import Data.Function (on)

sortArray :: [Int] -> [Int]
sortArray = map snd . sortBy (compare `on` fst) . uncurry (++) . first (uncurry zip . (sort *** sort) . unzip) . partition (odd . snd) . zip [1..]
