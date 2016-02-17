-- Find the smallest perfect power higher than a given a value
-- http://www.codewars.com/kata/56ba65c6a15703ac7e002075/

module Kata.SmallestPerfectPowerHigherThatAGivenValue where

findNextPower :: Integer -> Integer -> Integer
findNextPower n p = (^p) . ceiling $ (fromIntegral n)**(1 / fromIntegral p)
