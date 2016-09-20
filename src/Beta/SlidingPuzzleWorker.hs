-- Sliding tile puzzle - Worker
-- https://www.codewars.com/kata/57dc535fd8f9291a0300030d

module Haskell.Codewars.SlidingPuzzleWorker where

slideTiles :: [[Int]] -> [Int] -> [[Int]]
slideTiles = foldl slide

slide :: [[Int]] -> Int -> [[Int]]
slide xs i = map (map swap) xs
    where swap x | x == 0    = i
                 | x == i    = 0
                 | otherwise = x
