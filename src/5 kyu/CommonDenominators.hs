-- Common Denominators
-- http://www.codewars.com/kata/54d7660d2daf68c619000d95/

module Codewars.Kata.CommonDenominators where

type Ratio a = (a, a)

convertFracs :: Integral a => [Ratio a] -> [Ratio a]
convertFracs xs = zipWith(\n d -> (n * (cd `div` d), cd)) ns ds
    where (ns, ds) = unzip xs
          cd = foldl1 lcm ds
