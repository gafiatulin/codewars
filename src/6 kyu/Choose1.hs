-- Easy Diagonal
-- http://www.codewars.com/kata/559b8e46fa060b2c6a0000bf/

module Codewars.Kata.Choose1 where

diagonal n p = product [n-p+1..n+1] `div` product [1..p+1]
