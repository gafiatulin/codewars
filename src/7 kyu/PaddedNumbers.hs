-- Substituting Variables Into Strings: Padded Numbers
-- http://www.codewars.com/kata/51c89385ee245d7ddf000001

module Codewars.Kata.PaddedNumbers where

solution :: Int -> String
solution n = "Value is " ++ reverse (take 5 ((reverse . show $ n) ++ "00000"))
