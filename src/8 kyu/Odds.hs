-- Are arrow functions odd?
-- http://www.codewars.com/kata/559f80b87fa8512e3e0000f5/

module Odds where

odds :: [Int] -> [Int]
odds = filter odd
