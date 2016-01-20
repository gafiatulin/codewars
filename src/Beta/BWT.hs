-- Burrows-Wheeler-Transformation
-- http://www.codewars.com/users/gafiatulin

module BWT where

import Data.List (sort, elemIndex, inits, tails)
import Control.Arrow ((&&&))
import Data.Maybe (fromJust)

encode :: Ord a => [a] -> ([a], Int)
encode = (map last . snd &&& fromJust . uncurry ($)) . (elemIndex &&& sort . shifts)
    where shifts = reverse . tail . uncurry (zipWith (++)) . (tails &&& inits)

decode :: Ord a => [a] -> Int -> [a]
decode [] _ = []
decode xs n = f (length xs) !! n
    where f 0 = map (const []) xs
          f n = sort . zipWith (:) xs . f . pred $ n
