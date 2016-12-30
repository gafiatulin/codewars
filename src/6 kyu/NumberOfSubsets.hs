-- Estimating Amounts of Subsets
-- https://www.codewars.com/kata/584703d76f6cf6ffc6000275

module Kata (estSubsets) where

import Data.List (nub)

estSubsets :: Ord a => [a] -> Integer
estSubsets = pred . (2^) . length . nub
