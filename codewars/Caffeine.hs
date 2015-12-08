-- Caffeine Script
-- http://www.codewars.com/kata/5434283682b0fdb0420000e6

module Codewars.Kata.Caffeine where

caffeineBuzz :: Integer -> String
caffeineBuzz n | n `mod` 12 == 0 = "CoffeeScript"
               | n `mod`  6 == 0 = "JavaScript"
               | n `mod`  3 == 0 = "Java"
               | otherwise = "mocha_missing!"
