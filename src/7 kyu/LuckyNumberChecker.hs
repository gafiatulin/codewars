-- lucky number
-- http://www.codewars.com/kata/55afed09237df73343000042

module Codewars.LuckyNumberChecker where

isLucky :: Integer -> Bool
isLucky = (== 0) . (`mod` 9)