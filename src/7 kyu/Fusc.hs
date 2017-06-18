-- The fusc function -- Part 1
-- https://www.codewars.com/kata/570409d3d80ec699af001bf9

module Fusc where

fusc :: Int -> Int
fusc 0 = 0
fusc 1 = 1
fusc n | even n = fusc (n `div` 2)
       | otherwise = fusc (n `div` 2) + fusc (1 + (n `div` 2))
