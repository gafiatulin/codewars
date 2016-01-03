-- Fix it
-- http://www.codewars.com/kata/5443dd2d7fc4478154000ac6/

module Fixit where

import Prelude hiding (reverse, foldr)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f xs = if null xs then xs else f (tail xs) ++ [head xs]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' f g acc xs = if null xs then acc else g (head xs) (f g acc (tail xs))
