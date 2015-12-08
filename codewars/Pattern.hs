-- Complete The Pattern #4
-- http://www.codewars.com/kata/55736129f78b30311300010f

module Haskell.Codewars.Pattern where

pattern :: Int -> String
pattern n
    | n <= 0 = ""
    | otherwise = f 1 n 
    where f :: Int -> Int -> String
          f x y
              | x == y = show x
              | otherwise = (concat $ map show [x..y]) ++ "\n" ++ f (x+1) y
