-- Excessively Abundant Numbers
-- http://www.codewars.com/kata/56a75b91688b49ad94000015/

module Codewars.Numbers where

abundantNumber :: Int -> Bool
abundantNumber num = (> num) . sum . filter ((0 ==) . (num `mod`)) $ [1 .. (num `div` 2)]
