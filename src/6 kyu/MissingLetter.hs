-- Find the missing letter
-- https://www.codewars.com/kata/5839edaa6754d6fec10000a2

module Kata where

findMissingLetter :: String -> Char
findMissingLetter s@(x:_) = snd . head . dropWhile (uncurry (==)) . zip s $ [x..]
