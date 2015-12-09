-- Character Concatenation
-- http://www.codewars.com/kata/55147ff29cd40b43c600058b/
module Codewars.Kata.Catenation where

charConcat :: String -> String
charConcat word = foldr (\i str  -> word !! (i - 1) : word !! (length word - i) : show i ++ str) "" [1..length word `div` 2]
