-- All Star Code Challenge #1
-- https://www.codewars.com/kata/5863f97fb3a675d9a700003f

module Kata where

data Player = Player { team :: String, ppg :: Double } deriving (Show)

sumPpg :: Player -> Player -> Double
sumPpg p1 = (+ (ppg p1)) . ppg
