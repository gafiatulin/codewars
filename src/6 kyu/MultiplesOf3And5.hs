-- Multiples of 3 and 5
-- http://www.codewars.com/kata/514b92a657cdc65150000006/

module MultiplesOf3And5 where

solution :: Integer -> Integer
solution number = f number 3 + f number 5 - f number 15
      where f n k = k * ((n - 1) `div` k) * ((n - 1) `div` k + 1) `div` 2
