-- Mysterious function
-- http://www.codewars.com/kata/55217af7ecb43366f8000f76/

module Codewars.Kata.Mysterious where

getNum :: Integer -> Integer
getNum = foldr f 0 . show
    where f c | c `elem` "069" = succ
              | c == '8' = succ . succ
              | otherwise = id
