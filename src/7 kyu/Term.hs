-- Arithmetic Sequence!
-- http://www.codewars.com/kata/540f8a19a7d43d24ac001018/

module Term where

nthterm :: Int -> Int -> Int -> Int
nthterm first n c = first + n * c
