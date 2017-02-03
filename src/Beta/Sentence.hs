-- Sentence Count
-- https://www.codewars.com/kata/5884ee2465fc9c8dee0005e8

module Codewars.Kata.Sentence where

sentenceCount :: String -> Int
sentenceCount = length . filter (`elem` ".?!")
