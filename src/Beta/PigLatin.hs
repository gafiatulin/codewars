-- Igpay Atinlay
-- https://www.codewars.com/kata/58702c0ca44cfc50dc000245

module Kata(pigLatin) where

pigLatin :: String -> String
pigLatin [] = []
pigLatin (x:xs) | (>3) . succ . length $ xs = xs ++ [x] ++ "ay"
                | otherwise = x:xs
