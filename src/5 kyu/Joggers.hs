-- Two Joggers
-- http://www.codewars.com/kata/5274d9d3ebc3030802000165/

module Joggers where

nbrOfLaps :: Integer -> Integer -> (Integer, Integer)
nbrOfLaps bob charles = (\x -> (x `div` bob, x `div` charles)) . lcm bob $ charles
