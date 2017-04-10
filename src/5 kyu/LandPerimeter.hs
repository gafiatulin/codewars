-- Land perimeter
-- https://www.codewars.com/kata/5839c48f0cf94640a20001d3

module Kata where

import qualified Data.Set as Set

landPerimeter :: [String] -> String
landPerimeter = ("Total land perimeter: " ++) . show . perimeter

perimeter :: [String] -> Int
perimeter = snd . foldl f (Set.empty, 0) . concatMap (\(i, l) -> map (\(j, c) -> ((i, j), c)) . zip [0..] $ l) . zip [0..]

f :: (Set.Set (Int, Int), Int) -> ((Int, Int), Char) -> (Set.Set (Int, Int), Int)
f (s, c) ((i, j), 'X') = (Set.insert (i, j) s, (c +) . (4 -) . (*2) . Set.size . Set.intersection s . Set.fromList $ [(pred i, j), (i, pred j)])
f a (_, _) = a
