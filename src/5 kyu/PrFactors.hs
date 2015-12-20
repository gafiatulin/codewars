-- Primes in numbers
-- http://www.codewars.com/kata/54d512e62a5e54c96200019e/

module Codewars.Kata.PrFactors where

import Data.List (unfoldr, findIndex, group)
import Data.Maybe (fromJust)

prime_factors :: Integer -> String
prime_factors k | isPrime k = "("++ show k ++")"
                | otherwise = concatMap g . group . unfoldr f $ (k, 0)
    where primes = sieve [2..]
          sieve (n:ns) = n : sieve [n' | n' <- ns, n' `mod` n /= 0]
          isPrime x = all (\d -> x `mod` d  /= 0) [2 .. floor . sqrt . fromIntegral $ x]
          f (1, _) = Nothing
          f (n, i) | isPrime n = Just (n, (1, 0))
                   | otherwise = if m == 0 then Just (primes !! i , (d, i)) else Just (primes !! newI, (n `div` (primes !! newI), newI) )
              where (d, m) = n `divMod` (primes !! i)
                    newI :: Int
                    newI = succ . (+i) . fromJust . findIndex (\x-> n `mod` x  == 0) . drop (i+1) $ primes
          g :: [Integer] -> String
          g ps = "(" ++ (show . head $ ps) ++ (if length ps > 1 then "**" ++ (show . length $ ps) else "") ++ ")"
