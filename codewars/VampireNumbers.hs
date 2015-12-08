-- Vampire Numbers
-- http://www.codewars.com/kata/54d418bd099d650fa000032d

module Codewars.Kata.VampireNumbers where

import Data.List (sort)

isVampire :: Integer -> Integer -> Bool
isVampire a b = sort (show a ++ show b) == sort (show (a*b))
