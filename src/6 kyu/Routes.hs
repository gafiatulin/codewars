-- Routes in a square grid
-- http://www.codewars.com/kata/559aa1295f5c38fd7b0000ac/

module Kata.Routes where

routes :: Integer -> Integer
routes n = if n <= 0 then 0 else product [n+1..2*n] `div` product [1..n]
