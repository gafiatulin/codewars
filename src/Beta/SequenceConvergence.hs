-- Sequence convergence
-- https://www.codewars.com/kata/59971e64bfccc70748000068

module SequenceConvergence.Kata (convergence) where

import Data.Char (digitToInt)
import Data.List (find)
import Data.Maybe (fromJust)

convergence :: Int -> Int
convergence = length . takeWhile (\e -> fromJust . fmap ( /= e) . find (>= e) $ base) . iterate f

base = iterate f 1

f :: Int -> Int
f n | (== 1) . length . show $ n = n + n
    | otherwise = (+n) . product . filter (/= 0) . map digitToInt . show $ n
