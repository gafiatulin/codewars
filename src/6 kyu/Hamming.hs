-- Hamming Distance
-- http://www.codewars.com/kata/5410c0e6a0e736cf5b000e69/

module Hamming where

hamming :: String -> String -> Int
hamming a [] = length a
hamming [] b = length b
hamming (a:as) (b:bs) | a == b = hamming as bs
                      | otherwise = 1 + hamming as bs
