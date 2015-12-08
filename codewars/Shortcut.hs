-- Vowel remover
-- http://www.codewars.com/kata/5547929140907378f9000039

module Codewars.Kata.Shortcut where

shortcut :: String -> String
shortcut = filter (\c -> not $ elem c "aeiou")
