-- Counting sheep...
-- http://www.codewars.com/kata/54edbc7200b811e956000556

module Codewars.Kata.Sheep where

countSheep :: [Bool] -> Int
countSheep = length . (filter id)
