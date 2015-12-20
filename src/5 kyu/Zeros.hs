-- Number of trailing zeros of N!
-- http://www.codewars.com/kata/52f787eb172a8b4ae1000a34/

module Zeros where

zeros :: Int -> Int
zeros 0 = 0
zeros n = sum . tail . takeWhile (/=0) . iterate (`div` 5) $ n
