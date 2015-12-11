-- Printer Errors
-- http://www.codewars.com/kata/56541980fa08ab47a0000040/

module Codewars.G964.Printer where

printerError :: String -> String
printerError s = show ((+ length s) . negate . length . filter (`elem` ['a'..'m']) $ s) ++ "/" ++ show (length s)
