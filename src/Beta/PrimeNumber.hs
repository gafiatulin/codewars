-- Is prime number?
-- https://www.codewars.com/kata/57a1a3f153ba3315140013d5

module PrimeNumber where

isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = all (\d -> x `mod` d  /= 0) [2 .. floor . sqrt . fromIntegral $ x]
