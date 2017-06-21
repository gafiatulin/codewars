-- N-Point Crossover
-- http://www.codewars.com/kata/57339a5226196a7f90001bcf

module Kata.NPointCrossover where

import Data.List (nub, sort)
import Data.Tuple (swap)
import Control.Arrow ((***))

crossover :: [Int] -> [a] -> [a] -> ([a],[a])
crossover ns xs = unzip . f (nub . sort $ ns) 0 . zip xs
    where f [] i ps = map (ff !! (i `mod` 2)) ps
          f (n:ns) i ps = uncurry (++) . (map (ff !! (i `mod` 2)) *** f (map ( + (-n)) ns) (succ i)) . splitAt n $ ps
          ff = [id, swap]
