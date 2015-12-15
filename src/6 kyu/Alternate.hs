-- Alternating Loops
-- http://www.codewars.com/kata/55e529bf6c6497394a0000b5/

module Codewars.Kata.Alternate where

import Data.List (transpose)

combine :: [[a]] -> [a]
combine = concat . transpose
