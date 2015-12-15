-- Palindromic Numbers
-- http://www.codewars.com/kata/52a0f488852a85c723000aca/

module Palindromic where

palindromize :: Integer -> (Int, Integer)
palindromize x | (== show x) . reverse . show $ x = (0, x)
               | otherwise = (\(k, p) -> (k+1, p)) . palindromize . (+x) . read . reverse . show $ x
