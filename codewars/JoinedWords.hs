-- String array joining in Haskell
-- http://www.codewars.com/kata/5436bb1df0c10d280900131f

module JoinedWords where

joinS :: [String] -> String -> String
joinS l s = drop (length s) (foldl (\str w -> str ++ s ++ w) "" l)
