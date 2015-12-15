-- Backwards Read Primes
-- http://www.codewars.com/kata/5539fecef69c483c5a000015/

module Codewars.Kata.BackWardsPrime where

import Data.Array (accumArray, (!))


backwardsPrime :: Integer -> Integer -> [Integer]
backwardsPrime start stop = filter (\p -> (backwardsRead p /= p) && (isPrime . backwardsRead $ p)) primes
    where backwardsRead = read . reverse . show 
          isPrime n | n `elem` [2, 3, 5] = True
                    | n `mod` 2 == 0 || n `mod` 3 == 0 = False
                    | otherwise = notElem 0 . map (n `mod`) $ [5, 7 .. floor . sqrt . fromIntegral $ n]
          primes = primesBetween start stop
          primesBetween a b = [2 | a < 3] ++ [i | i <- [o,o+2..b], ar ! i]
              where o  = max (if even a then a + 1 else a) 3
                    r  = floor . sqrt $ fromIntegral b + 1
                    ar = accumArray (\_ _ -> False) 
                                    True
                                    (o, b)
                                    [(i,()) | p <- [3, 5..r]
                                    , let q  = p * p
                                          s  = 2 * p
                                          (n, x) = quotRem (o - q) s 
                                          q2 = if  o <= q  then q else  q + (n + signum x) * s
                                    , i <- [q2, q2+s..b]]
