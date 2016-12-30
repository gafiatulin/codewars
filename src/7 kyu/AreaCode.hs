-- Thinkful - String Drills: Areacode extractor
-- https://www.codewars.com/kata/585a36b445376cbc22000072

module Kata (areaCode) where

areaCode :: String -> String
areaCode = tail . takeWhile (/= ')') . dropWhile (/= '(')
