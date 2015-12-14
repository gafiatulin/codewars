-- Rank Vector
-- http://www.codewars.com/kata/545f05676b42a0a195000d95/

module RankVector where

import Data.List (sortBy, elemIndex)
import Data.Maybe (mapMaybe)

ranks :: (Eq a, Ord a) => [a] -> [Int]
ranks xs = map (+1) . mapMaybe (f xs) $ xs
    where f xs a = elemIndex a . sortBy (flip compare) $ xs
