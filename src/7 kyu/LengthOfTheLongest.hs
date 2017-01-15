-- Thinkful - List Drills: Longest word
-- https://www.codewars.com/kata/58670300f04e7449290000e5

module Kata (longest) where

longest :: [String] -> Int
longest = maximum . map length
