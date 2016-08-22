-- String repeat
-- https://www.codewars.com/kata/57a0e5c372292dd76d000d7e

module StringRepeat where

repeatStr :: Int -> String -> String
repeatStr n = concat . replicate n
