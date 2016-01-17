-- Multiples of Ten in a Sequence Which Values Climb Up
-- http://www.codewars.com/kata/561d54055e399e2f62000045/

module Codewars.Kata.ClimbUp where

import Control.Arrow ((&&&))

findMult10SF :: Int -> Integer
findMult10SF = uncurry (+) . ((*3) . (2^) &&& (*9) . (6^)) . (+ (-3)) . (*4)
