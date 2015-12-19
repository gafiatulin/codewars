-- Surrounding Primes for a value
-- http://www.codewars.com/kata/560b8d7106ede725dd0000e2/

module Codewars.G964.PrimBefAft where

import Control.Arrow ((***))

primeBefAft :: Integer -> (Integer, Integer)
primeBefAft n = (f *** f) (b, a)
    where b = if odd n then [n-2, n-4 ..] else [n-1, n-3 ..]
          a  = if odd n then [n+2, n+4 ..] else [n+1, n+3 ..]
          f = head . filter isPrime
          isPrime x = all (\d -> x `mod` d  /= 0) [3 .. floor . sqrt . fromIntegral $ x]
