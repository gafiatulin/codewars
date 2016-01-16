-- Decompose a number
-- http://www.codewars.com/kata/55ec80d40d5de30631000025/

module Kata.Decompose (decompose) where

import Data.List (unfoldr)
import Control.Arrow ((&&&))

decompose :: Integer -> ([Integer], Integer)
decompose n = (init &&& last) . unfoldr f $ (n, 2)
f (_, 0) = Nothing
f (s, n) = if k > 1 then Just (k, (s - n^k, succ n)) else Just (s, (0, 0))
    where k = head . dropWhile ((>=n) . (s `div`) . (n^)) $ [1..]
