-- Range Extraction
-- https://www.codewars.com/kata/51ba717bb08c1cd60f00002f

module RangeExtractor.JorgeVS.Kata (solution) where

import Data.List (intercalate, unfoldr)

data R = Single Int | Range Int Int

instance Show R where
    show (Single n) = show n
    show (Range s e) = show s ++ "-" ++ show e

solution :: [Int] -> String
solution = intercalate "," . map show . unfoldr f

f [] = Nothing
f (x:xs) = let ss = g xs [x+1..] in Just (if length ss >= 2 then (Range x (last ss), dropWhile (<= last ss) xs) else (Single x, xs))

g [] _ = []
g (x:xs) (y:ys) | x == y =  x : g xs ys
                | otherwise = []
