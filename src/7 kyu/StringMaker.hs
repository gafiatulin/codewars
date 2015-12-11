-- Return String of First Characters
-- http://www.codewars.com/kata/5639bdcef2f9b06ce800005b

module Codewars.StringMaker where

makeString :: String -> String
makeString = map head . words
