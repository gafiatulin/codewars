-- House of cards
-- http://www.codewars.com/kata/543abbc35f0461d28f000c11/

module Codewars.Cards where

houseOfCards :: Integer -> Maybe Integer
houseOfCards n | n <= 0 = Nothing
               | otherwise = Just ((n+1)*(3*n+4) `div` 2)
