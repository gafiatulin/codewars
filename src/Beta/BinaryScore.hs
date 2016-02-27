-- Binary scORe
-- http://www.codewars.com/kata/56cafdabc8cfcc3ad4000a2b

module Kata.BinaryScore where

score :: Integer -> Integer
score n = pred . head . dropWhile (<= n) . map (2^) $ [0..]
