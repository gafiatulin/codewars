-- Financing Plan on Planet XY140Z-n
-- http://www.codewars.com/kata/559ce00b70041bc7b600013d/

module Codewars.Kata.Finance where

finance :: Integer -> Integer
finance n = (n * (n+1) * (n+2)) `div` 2
