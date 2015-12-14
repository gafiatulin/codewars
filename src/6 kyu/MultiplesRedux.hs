-- Multiples of 3 and 5 redux
-- http://www.codewars.com/kata/54bb6ee72c4715684d0008f9/

module Codewars.Kata.MultiplesRedux where

solution :: Integer -> Integer
solution number = f number 3 + f number 5 - f number 15
      where f n k = k * ((n - 1) `div` k) * ((n - 1) `div` k + 1) `div` 2
