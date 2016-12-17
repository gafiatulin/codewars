-- Sorted? yes? no? how?
-- https://www.codewars.com/kata/580a4734d6df748060000045

module Kata where

isSortedAndHow :: [Int] -> String
isSortedAndHow l | and . zipWith (<=) l . tail $ l = "yes, ascending"
                 | and . zipWith (>=) l . tail $ l = "yes, descending"
                 | otherwise = "no"
