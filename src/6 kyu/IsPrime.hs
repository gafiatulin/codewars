-- Is a number prime?
-- http://www.codewars.com/kata/5262119038c0985a5b00029f/

module IsPrime where

import Data.Maybe (listToMaybe)
import Data.List (unfoldr)

isPrime :: Integer -> Bool
isPrime = isPrime' . abs
  where pfactors prs n = unfoldr (\(ds,n) -> listToMaybe [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++ [ n | n > 1 ], mod n x == 0]) (prs,n)
        primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]
        isPrime' n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
