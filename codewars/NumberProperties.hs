-- Simple Maths Test
-- http://www.codewars.com/kata/5507309481b8bd3b7e001638

module Codewars.Kata.NumberProperties where

import Codewars.Kata.NumberProperties.Property
import Data.List (unfoldr)
import Data.Maybe (listToMaybe)

-- data Property = Property Bool   Bool  Bool
--                          prime  even  10*

numberProperty :: Integral n => n -> Property
numberProperty n | n < 0     = Property False (even n) (n `mod` 10 == 0)
                 | n == 2    = Property True True False
                 | even n    = Property False True (n `mod` 10 == 0)
                 | otherwise = Property (isPrime n) False False
                 where pfactors prs n = unfoldr (\(ds,n) -> listToMaybe [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++ [ n | n > 1 ], mod n x == 0]) (prs,n)
                       primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]
                       isPrime n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
