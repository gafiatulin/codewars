-- Sieve of Eratosthenes
-- http://www.codewars.com/kata/55f0b69fe3ef582c4100008a/

module Sieve where

primes :: Int -> [Int]
primes n | n <= 1 = []
         | otherwise = filter isPrime $ 2:[3, 5..n]
         where isPrime n = all (\d -> n `mod` d  /= 0) [2 .. floor . sqrt . fromIntegral $ n]
