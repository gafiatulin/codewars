-- SevenAte9
-- http://www.codewars.com/kata/559f44187fa851efad000087

module Codewars.Exercise.SevenAte9 where

sevenAte9 :: String -> String
sevenAte9 [] = []
sevenAte9 ('7':'9':'7':xs) = "7" ++ sevenAte9 ('7':xs)
sevenAte9 (x:xs) = x : sevenAte9 xs
