-- 1's, 0's and wildcards
-- https://www.codewars.com/kata/588f3e0dfa74475a2600002a

module Kata (possibilities) where

possibilities :: String -> [String]
possibilities = f [""]

f :: [String] -> String -> [String]
f acc [] = acc
f acc (x:xs) | x == '?' = f (concatMap (\x -> [x ++ "0", x ++ "1"]) acc) xs
             | otherwise = f (map (++ [x]) acc) xs
