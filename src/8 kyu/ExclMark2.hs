-- Exclamation marks series #2: Remove all exclamation marks from the end of sentence
-- https://www.codewars.com/kata/57faece99610ced690000165

module Kata where

remove :: String -> String
remove = reverse . dropWhile (=='!') . reverse
