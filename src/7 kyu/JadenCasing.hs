-- Jaden Casing Strings
-- http://www.codewars.com/kata/5390bac347d09b7da40006f6

module JadenCasing where

import Data.Char (toUpper)

toJadenCase :: String -> String
toJadenCase str = unwords $ map (\w -> toUpper (head w) : tail w) (words str)
