module Codewars.Kata.SortedUnion where

import Data.List (nub)

uniteUnique :: Ord a => [[a]] -> [a]
uniteUnique = nub . concat