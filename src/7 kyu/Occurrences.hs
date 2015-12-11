-- Number Of Occurrences
-- http://www.codewars.com/kata/52829c5fe08baf7edc00122b

module Codewars.Occurrences where

numberOfOccurrences :: Eq a => a -> [a] -> Int
numberOfOccurrences x = length . filter (== x)
