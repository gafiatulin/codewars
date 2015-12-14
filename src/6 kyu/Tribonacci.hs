-- Tribonacci Sequence
-- http://www.codewars.com/kata/556deca17c58da83c00002db/

module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n tribs
    where tribs = a:b:c:(zipWith3 (\a b c -> a+b+c) tribs (tail tribs) (tail . tail $ tribs))
