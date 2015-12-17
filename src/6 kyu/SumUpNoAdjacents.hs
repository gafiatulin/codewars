-- Sum Up to Target Non Adjacents elements
-- http://www.codewars.com/kata/54554526126a00dbc2000823/

module SumUpNoAdjacents where

sumUpNoAdjacents :: [Int] -> Int -> Bool
sumUpNoAdjacents _ 0 = True
sumUpNoAdjacents [] _ = False
sumUpNoAdjacents (x:xs) s | x == s = True
                          | otherwise = (not . null $ xs) && (sumUpNoAdjacents (tail xs) (s - x) || sumUpNoAdjacents xs s)
