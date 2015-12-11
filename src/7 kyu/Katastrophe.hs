-- Katastrophe!
-- http://www.codewars.com/kata/55a3cb91d1c9ecaa2900001b

module Codewars.Kata.Katastrophe where

strongEnough :: [[Int]] -> Int -> String
strongEnough earthquake age = if (fromIntegral . product $ map sum earthquake) <= 1000.0 * (0.99 ^ age) 
                              then "Safe!" 
                              else "Needs Reinforcement!"
