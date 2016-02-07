-- Double Char
-- http://www.codewars.com/kata/56b1f01c247c01db92000076/

module Codewars.Strings where

doubleChar :: String -> String
doubleChar = concatMap (\c -> [c,c])
