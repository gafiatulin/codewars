-- Backspaces in string
-- https://www.codewars.com/kata/5727bb0fe81185ae62000ae3

module Codewars.BackspacesInString where

cleanString :: String -> String
cleanString = reverse . foldl (\str c -> if c == '#' then drop 1 str else c:str) ""
