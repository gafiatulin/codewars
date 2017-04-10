-- Fold an array
-- https://www.codewars.com/kata/57ea70aa5500adfe8a000110

module Folded.MyLists where

foldList :: [Int] -> Int -> [Int]
foldList a 0 = a
foldList xs n = foldList (f xs) (pred n)
    where f xs = let l = (`div` 2) . length $ xs in zipWith (+) ((++ repeat 0) . take l $ xs) (reverse . drop l $ xs)
