-- Pure odd digits primes
-- https://www.codewars.com/kata/55e0a2af50adf50699000126

module Codewars.G964.Pureoddprime(onlyOddDigPrime) where

import Data.Maybe (listToMaybe)
import Data.List (genericLength, unfoldr)
import Data.Char (digitToInt)

primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]
pfactors prs n = unfoldr (\(ds,n) -> listToMaybe [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++ [ n | n > 1 ], mod n x == 0]) (prs,n)

onlyOddDigPrime :: Integer -> [Integer]
onlyOddDigPrime n = [genericLength s, last s, if head b == n then head . tail $ b else head b]
    where (s, b) = span (< n) . filter pureOdd $ primes
          pureOdd = all odd . map digitToInt . show
