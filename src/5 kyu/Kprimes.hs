-- k-Primes
-- https://www.codewars.com/kata/5726f813c8dcebf5ed000a6b

module Codewars.G964.Kprimes(countKprimes, puzzle) where

factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
           in (prime :) $ factor $ div n prime

countKprimes :: Int -> Int -> Int -> [Int]
countKprimes k start end = filter ((==k) . length . factor) [start .. end]

puzzle :: Int -> Int
puzzle n = length [1 | a <- as, b <- bs, c <- cs, a + b + c == n]
    where as = countKprimes 1 2 n
          bs = countKprimes 3 2 n
          cs = countKprimes 7 2 n
