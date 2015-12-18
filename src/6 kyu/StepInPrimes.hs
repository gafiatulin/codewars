-- Steps in Primes
-- http://www.codewars.com/kata/5613d06cee1e7da6d5000055/

module Codewars.G964.StepInPrimes where

import Data.List (find)
import Control.Monad (liftM)

step :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)

step g m n = liftM (\p -> (p, p+g)) . find (\v -> isPrime v && isPrime (v+g)) $ [m..n]
    where isPrime n = elem n [2,3] || (n < 25 && ((n - 1) `mod` 6 == 0 || (n + 1) `mod` 6 == 0)) || not (any ((== 0) . (n `mod`)) $ takeWhile ((<= n) . (^ 2)) primes)
          primes = sieve (2 : 3 : possible [1..])
          sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
          possible (x:xs) = 6*x-1 : 6*x+1 : possible xs
