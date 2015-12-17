-- Frequency Frenzy 2: Endure equal elements
-- http://www.codewars.com/kata/544a2d3818b8e00084000336/

module FrequencyFrenzy where

import Data.List (unfoldr, partition)

frequency :: Eq a => [a] -> [(a, Int)]
frequency = unfoldr (\rest -> if null rest then Nothing else Just . (\(t, f) -> ((head rest, succ . length $ t) , f)) . partition (== head rest) . tail $ rest)
