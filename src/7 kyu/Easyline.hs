-- Easy Line
-- http://www.codewars.com/kata/56e7d40129035aed6c000632

module Codewars.Kata.Easyline where

easyLine :: Integer -> Integer
easyLine n = product [n+1..2*n] `div` product [1..n]