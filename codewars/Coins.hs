-- The Coins of Ter | Round to the Next N
-- http://www.codewars.com/kata/55d38b959f9c33f3fb00007d/

module Codewars.Kata.Coins where

adjust :: Int -> Int -> Int
adjust coin price | price `mod` coin == 0 = price
                  | otherwise = coin * (price `div` coin + 1)
