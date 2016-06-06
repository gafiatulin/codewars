-- Upper <body> Strength
-- http://www.codewars.com/kata/571640812ad763313600132b

module Codewars.Kata.Strength where

alexMistakes :: Int -> Int -> Int
alexMistakes numberOfKatas timeLimit = floor . logBase 2 . (/ 5.0) . fromIntegral $ (timeLimit - (6 * numberOfKatas))
