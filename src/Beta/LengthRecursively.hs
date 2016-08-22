-- Get length of the list recursively
-- https://www.codewars.com/kata/57a83e447cb1f32de80000d5

module LengthRecursively where

lenR [] = 0
lenR (_:xs) = 1 + lenR xs
