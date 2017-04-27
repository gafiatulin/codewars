-- Simple Fun #66: Obtain Max Number
-- https://www.codewars.com/kata/5893f03c779ce5faab0000f6

module Kata where

import Data.List (sort)

obtain_max_number :: [Int] -> Int
obtain_max_number [] = 0
obtain_max_number l | (== l) . f . sort $ l = last l
                    | otherwise = obtain_max_number . f . sort $ l
                    where f (x:y:xs) = if x == y then (x+y) : f xs else x : f (y:xs)
                          f x = x
