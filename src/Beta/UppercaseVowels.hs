-- Uppercase all vowels
-- https://www.codewars.com/kata/57a1e23853ba339caf001000

module UppercaseVowels where
import Data.Char(toUpper)

uppercaseVowels :: String -> String
uppercaseVowels = map (\c -> if c `elem` "aeiou" then toUpper c else c)
