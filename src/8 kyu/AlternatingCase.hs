-- altERnaTIng cAsE <=> ALTerNAtiNG CaSe
-- http://www.codewars.com/kata/56efc695740d30f963000557

module Codewars.Kata.AlternatingCase where

import Data.Char(isLower, toUpper, toLower)

toAlternatingCase :: String -> String
toAlternatingCase = map (\x -> if isLower x then toUpper x else toLower x)