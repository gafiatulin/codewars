-- Disemvowel Trolls
-- http://www.codewars.com/kata/52fba66badcd10859f00097e

module Disemvowel where

import Data.Char (toLower)

disemvowel :: String -> String
disemvowel = filter (\c -> not $ elem (toLower c) "aeiou")
