-- Prime Sextuplets
-- https://www.codewars.com/kata/57bf7fae3b3164dcac000352

module PrimesSextuplets (findPrimesSextuplet) where

import Data.List(tails)
import Data.Array.Unboxed (UArray, accumArray, assocs)

findPrimesSextuplet :: Int -> [Int]
findPrimesSextuplet sumLimit = head . filter isSextuplet . window6 . dropWhile (< ((sumLimit - 48) `div` 6)) $ primes
    where window6 = foldr (zipWith (:)) (repeat []) . take 6 . tails
          isSextuplet (x:xs) = (== [0, 4, 6, 10, 12, 16]) . (0:) . map (\n -> n-x) $ xs

primes :: [Int]
primes = 2 : oddprimes ()
    where oddprimes () = 3 : sieve (oddprimes ()) 3 []
          sieve (p:ps) x fs = [i*2 + x | (i,True) <- assocs a] ++ sieve ps (p*p) ((p,0) : [(s, rem (y-q) s) | (s,y) <- fs])
            where q = (p*p-x)`div`2
                  a :: UArray Int Bool
                  a = accumArray (\ b c -> False) True (1,q-1) [(i,()) | (s,y) <- fs, i <- [y+s, y+s+s..q]]
