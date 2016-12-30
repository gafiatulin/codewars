-- Exclamation marks series #13: Count the number of exclamation marks and question marks, return the product
-- https://www.codewars.com/kata/57fb142297e0860073000064

module Kata (product') where

product' :: String -> Int
product' s = let f c s = length . filter (== c) $ s in  (f '!' s) * (f '?' s)
