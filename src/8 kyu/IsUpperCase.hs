-- Is the string uppercase?
-- http://www.codewars.com/kata/56cd44e1aa4ac7879200010b

module Codewars.Kata.IsUpperCase where
import Data.Char (isLower)

isUpperCase :: String -> Bool
isUpperCase = all (not . isLower)