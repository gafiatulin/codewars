-- Consecutive k-Primes
-- https://www.codewars.com/kata/573182c405d14db0da00064e

module Codewars.G964.Primeconsec(consecKprimes) where

factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
           in (prime :) $ factor $ div n prime

consecKprimes k xs = sum . zipWith (\a b -> if a == b && a == k then 1 else 0) ls . tail $ ls
    where ls = map (length . factor) xs
