-- Shortest list, lazily
-- https://www.codewars.com/kata/585a8bd717cc9027e3000274

module ShortestList where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

shortestList :: [[a]] -> [a]
shortestList [] = []
shortestList xs = let i ys = fromMaybe (i (map tail ys)) . findIndex null $ ys in xs !! i xs
