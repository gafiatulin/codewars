-- ROT13
-- https://www.codewars.com/kata/57a30f5153ba334ce100009b

module ROT13 where

import Data.Char (chr, ord)

rot13 :: String -> String
rot13 = map f
    where shift c a s = chr . (ord a +) . (`mod` 26) $ (ord c - ord a + s)
          f c | c `elem` ['a'..'z'] = shift c 'a' 13
              | c `elem` ['A'..'Z'] = shift c 'A' 13
              | otherwise = c
