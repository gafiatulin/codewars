-- Find divisors number
-- http://www.codewars.com/kata/542c0f198e077084c0000c2e

module Divisors where

divisors :: Integral a => a -> Int
divisors x = length . filter (\n -> x `mod` n == 0) $ [1..x]
