-- Powers of 2
-- https://www.codewars.com/kata/57a083a57cb1f31db7000028

module PowersOfTwo where

powersOfTwo :: Int -> [Int]
powersOfTwo n = map (2 ^) [0..n]
