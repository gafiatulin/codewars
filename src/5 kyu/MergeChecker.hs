-- Merged String Checker
-- http://www.codewars.com/kata/54c9fcad28ec4c6e680011aa/

module Codewars.Exercise.MergeChecker where

isMerge :: String -> String -> String -> Bool
isMerge "" "" "" = True
isMerge "" _ _ = False
isMerge s part1 "" = s == part1
isMerge s "" part2 = s == part2
isMerge (s:ss) (x:xs) (y:ys) | x == y && x == s = isMerge ss xs (y:ys) || isMerge ss (x:xs) ys
                             | s == x = isMerge ss xs (y:ys)
                             | s == y = isMerge ss (x:xs) ys
                             | otherwise = False
