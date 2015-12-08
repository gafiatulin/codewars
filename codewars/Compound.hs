-- CompoundArray
-- http://www.codewars.com/kata/56044de2aa75e28875000017

module Codewars.Kata.Compound where

compoundArray :: [a] -> [a] -> [a]
compoundArray xs [] = xs
compoundArray [] ys = ys
compoundArray (x:xs) (y:ys) = x:y:compoundArray xs ys
