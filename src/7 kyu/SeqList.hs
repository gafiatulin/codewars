-- Arithmetic List!
-- http://www.codewars.com/kata/541da001259d9ca85d000688

module SeqList where

seqlist :: Int -> Int -> Int -> [Int]
seqlist f c l = take l [f, f+c ..]
