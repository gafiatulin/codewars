-- Josephus Permutation
-- https://www.codewars.com/kata/5550d638a99ddb113e0000a2

module Josephus where

import Data.List(unfoldr)

josephus :: [a] -> Int -> [a]
josephus xs k = unfoldr f (k - 1, xs)
    where f (_, []) = Nothing
          f (i, xs) = let p = i `mod` length xs in Just (xs !! p, (p + k - 1, take p xs ++ drop (p+1) xs))
