-- Irreducible Sum of Rationals
-- http://www.codewars.com/kata/5517fcb0236c8826940003c9/

module Codewars.Kata.SumFracts where

sumFracts :: [(Integer, Integer)] -> Maybe String
sumFracts [] = Nothing
sumFracts xs = Just $ if numerator' `mod` denominator' == 0 
                      then show (numerator' `div` denominator') 
                      else show (numerator' `div` gcd') ++ " " ++ show (denominator' `div` gcd')
    where unique [] = []
          unique (x: xs) = x : unique (filter (x/=) xs)
          (numerators, denominators) = unzip xs
          numerator' =  sum . map (\(n, d) -> (*n) . product . filter (/= d) $ denominators) $ xs
          denominator' = product . unique $ denominators
          gcd' = gcd numerator' denominator'
