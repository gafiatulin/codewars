-- Number climber
-- http://www.codewars.com/kata/559760bae64c31556c00006b

module Codewars.NumberClimber where

climb :: Int -> [Int]
climb = reverse . takeWhile (>=1) . iterate (`div` 2)