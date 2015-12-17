-- Disgruntled Employee
-- http://www.codewars.com/kata/541103f0a0e736c8e40011d5/

module Codewars.Kata.Employee where

off :: Integer -> [Integer]
off n = takeWhile (<=n) . map (^2) $ [1..]
