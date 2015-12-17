-- Scheduling (Shortest Job First or SJF)
-- http://www.codewars.com/kata/550cc572b9e7b563be00054f/

module Codewars.Kata.SJF where

import Data.List (sort)

type CC    = Integer
type Job   = CC
type Index = Int

sjf :: [Job] -> Index -> CC
sjf xs i = sum . (\(before, after) -> (fst . head $ after) : map fst before ) . span ((/=i) . snd) . sort . zip xs $ [0..]
