-- Permutation position
-- https://www.codewars.com/kata/57630df805fea67b290009a3

module Kata where

import Data.List(foldl')
import Data.Char (ord)

permutationPosition :: String -> Int
permutationPosition = succ . foldl' (\acc c -> 26*acc + ord c - ord 'a') 0
