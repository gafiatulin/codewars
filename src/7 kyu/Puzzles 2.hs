-- Thinking & Testing : Something capitalized
-- https://www.codewars.com/kata/56d93f249c844788bc000002

module Codewars.Puzzles(testit) where
import Data.Char (toUpper)

testit :: String -> String
testit = unwords . map capitalize . words
  where capitalize [] = []
        capitalize (x:xs) = (toUpper x): xs
