-- Perimeter of a Rectangle
-- https://www.codewars.com/kata/58424c2e5692f50715000080

module Kata where

getPerimeter :: Int -> Int -> Int
getPerimeter w = (*2) . (+w)
