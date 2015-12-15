-- Hanoi record
-- http://www.codewars.com/kata/534eb5ad704a49dcfa000ba6/

module Hanoi where

hanoi :: Int -> Int
hanoi n = 2 ^ n - 1
