-- Get the mean of an array
-- http://www.codewars.com/kata/563e320cee5dddcf77000158

module Codewars.AverageCalculator where

getAverage :: [Int] -> Int
getAverage marks = sum marks `div` length marks 
