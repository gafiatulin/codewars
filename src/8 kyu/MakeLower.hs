-- MakeLowerCase
-- https://www.codewars.com/kata/57a059d753ba33229500001a

module MakeLower where

import Data.Char (toLower)

makeLowerCase :: String -> String
makeLowerCase = map toLower
