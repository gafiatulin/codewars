-- Special Multiples
-- http://www.codewars.com/kata/55e785dfcb59864f200000d9

module Codewars.G964.Countmult where

import Data.Maybe (listToMaybe)
import Data.List (unfoldr)

countSpecMult :: Int -> Integer -> Integer
countSpecMult n maxval = (maxval `div`) . product . take n $ primes
    where primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]
          pfactors prs n = unfoldr (\(ds,n) -> listToMaybe [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++ [ n | n > 1 ], mod n x == 0]) (prs,n)
