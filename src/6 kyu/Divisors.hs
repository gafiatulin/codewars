-- Find the divisors!
-- http://www.codewars.com/kata/544aed4c4a30184e960010f4/

module Divisors where

divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a = case filter (\d -> a `mod` d == 0) [2 .. a `div` 2] of
    [] -> Left(show a ++ " is prime")
    xs -> Right xs
