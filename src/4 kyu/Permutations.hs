-- Permutations
-- http://www.codewars.com/kata/5254ca2719453dcc0b00027d/

module Codewars.Kata.Permutations (permutations) where

import Data.List (nub)

permutations :: String -> [String]
permutations = nub . perm

perm [] = [[]]
perm [x] = [[x]]
perm (x:xs) = concatMap (\ys -> map ((\(a, b) -> a++x:b) . (`splitAt` ys)) [0..length ys]) . perm $ xs
