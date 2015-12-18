-- Integers: Recreation One
-- http://www.codewars.com/kata/55aa075506463dac6600010d/

module Codewars.G964.Sumdivsq where

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = map (\i -> (i, sOfSOfD i) ) . filter (\i -> isSquare . sOfSOfD $ i) $ [m..n]
    where divisors n = 1 : filter ((==0) . rem n) [2 .. floor . sqrt . fromIntegral $ n]
          sOfSOfD 1 = 1
          sOfSOfD n = sum . map (\a -> if a^2 == n then n else (a^4 + n^2) `div` (a^2)) . divisors $ n
          isSquare x = (==x) . (^2) . round . sqrt . fromIntegral $ x
