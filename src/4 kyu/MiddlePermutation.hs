-- Simple Fun #159: Middle Permutation
-- https://www.codewars.com/kata/58ad317d1541651a740000c5

module MiddlePermutation.JorgeVS.Kata where

import Data.List (sort, splitAt)

middlePermutation :: String -> String
middlePermutation s = if o == 1 then b : last a : reverse (init a ++ bs) else last a : reverse (init a ++ b:bs)
    where (a, b:bs) = splitAt i . sort $ s
          (i, o) = (`divMod` 2) . length $ s
