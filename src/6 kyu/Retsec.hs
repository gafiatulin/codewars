-- Do you speak retsec?
-- http://www.codewars.com/kata/5516ab668915478845000780/

module Codewars.Kata.Retsec where

reverseByCenter :: String -> String
reverseByCenter xs = f . splitAt (length xs `div` 2) $ xs
  where f (f, s) | length f == length s = s++f
                 | otherwise = tail s ++ [head s] ++ f
