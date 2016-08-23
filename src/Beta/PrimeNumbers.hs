-- Get n first prime numbers
-- https://www.codewars.com/kata/57a1a89fcf1fa56b8600154a

module PrimeNumbers where

import Data.Maybe (listToMaybe)
import Data.List (unfoldr)

getPrimes :: Int -> [Int]
getPrimes n = take n primes
  where pfactors prs n = unfoldr (\(ds,n) -> listToMaybe [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++ [ n | n > 1 ], mod n x == 0]) (prs,n)
        primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]
