-- (Ready for) Prime Time
-- http://www.codewars.com/kata/521ef596c106a935c0000519/

module PrimeTime where

prime :: Int -> [Int]
prime n = takeWhile (<= n) . sieve $ [2..]
    where sieve (n:ns) = n : sieve [n' | n' <- ns, n' `mod` n /= 0]
