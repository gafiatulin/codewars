-- Unique Pairs
-- http://www.codewars.com/kata/540c013634e6bac0350000a5/

module Codewars.Kata.Unique where

projectPartners :: Integer -> Integer
projectPartners n = n * (n-1) `div` 2
