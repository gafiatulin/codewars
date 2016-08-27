-- Price of Mangoes
-- https://www.codewars.com/kata/57a77726bb9944d000000b06

module PriceOfMango where

mango :: Int -> Int -> Int
mango q p = (q `quot` 3) * (2*p) + (q `mod` 3) * p
