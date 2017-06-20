-- Find the unique number
-- https://www.codewars.com/kata/585d7d5adb20cf33cb000235

module FindUniqueDouble.JorgeVS.Kata where

findUnique :: [Double] -> Double
findUnique (x:y:z:xs) | (x == y) && (y == z) = head . filter (not . (== x)) $ xs
                      | y == z = x
                      | x == z = y
                      | otherwise = z
