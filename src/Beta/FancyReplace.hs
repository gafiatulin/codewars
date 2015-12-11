-- Replace multiples with string
-- http://www.codewars.com/kata/546deb27018e9559470000b5

module Codewars.Kata.FancyReplace where

getNumber :: Integer -> Either Integer  String
getNumber n | n `mod` 15 == 0 = Right "BOTH"
            | n `mod`  5 == 0 = Right "FIVE"
            | n `mod`  3 == 0 = Right "THREE"
            | otherwise = Left n

getNumberRange :: Integer -> Integer -> [Either Integer String]
getNumberRange first last = [getNumber x | x <- if first <= last then [first .. last] else [first, first-1 .. last]]
