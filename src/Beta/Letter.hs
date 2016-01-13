-- Letter Changes
-- http://www.codewars.com/kata/5530b10808541c24330000b4/

module Codewars.Kata.Letter where

letterChange :: String -> String
letterChange = map f
    where f c | c == 'z' = 'a'
              | c == 'Z' = 'A'
              | c `elem` (['a'..'y'] ++ ['A'..'Y']) = succ c
              | otherwise = c
