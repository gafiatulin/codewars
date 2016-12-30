-- Ordered Count of Characters
-- https://www.codewars.com/kata/57a6633153ba33189e000074

module Kata where

import Data.List (partition)

orderedCount :: String -> [(Char, Int)]
orderedCount [] = []
orderedCount (x:xs) = let (s, d) = partition (== x) xs in (x, succ . length $ s) : orderedCount d
