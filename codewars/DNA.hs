-- Complementary DNA
-- http://www.codewars.com/kata/554e4a2f232cdd87d9000038

module Codewars.Kata.DNA where

import Codewars.Kata.DNA.Types

-- data Base = A | T | G | C
type DNA = [Base]

dnaStrand :: DNA -> DNA
dnaStrand [] = []
dnaStrand (A:xs) = T : dnaStrand xs 
dnaStrand (T:xs) = A : dnaStrand xs 
dnaStrand (C:xs) = G : dnaStrand xs 
dnaStrand (G:xs) = C : dnaStrand xs
