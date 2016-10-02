-- Sudoku Solver
-- https://www.codewars.com/kata/5296bc77afba8baa690002d7

module Sudoku where

import Data.List.Split (chunksOf)

itop :: Int -> (Int, Int)
itop i = (i - 9 * (i `div` 9), i `div` 9)

ptoi :: (Int, Int) -> Int
ptoi (x, y) = x + y * 9

columnAt :: [Int] -> Int -> [Int]
columnAt s = (\x -> map (\y -> s !! ptoi (x, y)) [0..8]) . fst . itop

rowAt :: [Int] -> Int -> [Int]
rowAt s = (\y -> map (\x -> s !! ptoi (x, y)) [0..8]) . snd . itop

squareAt :: [Int] -> Int -> [Int]
squareAt s = (\(x, y) -> [ s !! ptoi (xx + 3 * (x `div` 3), yy + 3 * (y `div` 3)) | xx <- [0..2], yy <- [0..2] ]) . itop

remove :: [Int] -> [Int] -> [Int]
remove l r = filter (`notElem` r) l

solutionsAt :: Int -> [Int] -> [Int]
solutionsAt p s | p > length s  = []
                | (s !! p) == 0 = [1..9] `remove` (columnAt s p ++ rowAt s p ++ squareAt s p)
                | otherwise = [s !! p]

tryWith :: Int -> [Int] -> Int -> [Int]
tryWith p s x = take p s ++ [x] ++ drop (p + 1) s

nextBlank :: Int -> [Int] -> Int
nextBlank p s | p == 80 = 80
              | s !! (p + 1) == 0 = p + 1
              | otherwise = nextBlank (p + 1) s

solve :: Int -> [Int] -> [Int] -> [Int]
solve 80 s [] = []
solve 80 s [x] = tryWith 80 s x
solve 80 s (x:_) = []
solve _  s [] = []
solve p s (x:xs)  | null solvedNext = solve p s xs
                  | otherwise = solvedNext
                  where solveNext p s = solve (nextBlank p s) s (solutionsAt (nextBlank p s) s)
                        solvedNext = solveNext p (tryWith p s x)

solveIt s = solve 0 s (solutionsAt 0 s)

sudoku :: [[Int]] -> [[Int]]
sudoku = chunksOf 9 . solveIt . concat
