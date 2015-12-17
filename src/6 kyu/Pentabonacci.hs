-- Pentabonacci
-- http://www.codewars.com/kata/55c9172ee4bb15af9000005d/

module Codewars.Kata.Pentabonacci where

countOddPentaFib :: Int -> Int
countOddPentaFib 0 = 0
countOddPentaFib 1 = 1
countOddPentaFib n = pred . length . takeWhile (<=n) . map (\i -> (if odd i then succ else pred) . (*3) . pred $ i) $ [1..]
