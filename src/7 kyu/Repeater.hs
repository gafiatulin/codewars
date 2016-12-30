-- Thinkful - String Drills: Repeater
-- https://www.codewars.com/kata/585a1a227cb58d8d740001c3

module Kata where

repeater :: String -> Int -> String
repeater s n = concat . replicate n $ s
