-- MakeUpperCase
-- https://www.codewars.com/kata/57a0556c7cb1f31ab3000ad7

module MakeUpper where

import Data.Char (toUpper)

makeUpperCase :: String -> String
makeUpperCase = map toUpper
