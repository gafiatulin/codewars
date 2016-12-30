-- They say that only the name is long enough to attract attention. They also said that only a simple Kata will have someone to solve it. This is a sadly story #1: Are they opposite?
-- https://www.codewars.com/kata/574b1916a3ebd6e4fa0012e7

module Kata where

import Data.Char(isUpper, toLower, toUpper)

isOpposite :: String -> String -> Bool
isOpposite s1 | null s1 = const False
              | otherwise = (== s1) . map f
              where f x = if isUpper x then toLower x else toUpper x
