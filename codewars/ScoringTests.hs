-- Scoring Tests
-- http://www.codewars.com/kata/55d2aee99f30dbbf8b000001

module Codewars.Kata.ScoringTests where

scoreTest :: (Integral a) => [a] -> a -> a -> a -> a
scoreTest li a b c = sum (map f li) where f 0 = a
                                          f 1 = b
                                          f 2 = -c
