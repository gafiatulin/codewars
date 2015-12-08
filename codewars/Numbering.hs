-- Testing 1-2-3
-- http://www.codewars.com/kata/54bf85e3d5b56c7a05000cf9

module Numbering where

number :: [String] -> [String]
number = zipWith (\x y -> show x ++ ": " ++ y) [1..]
