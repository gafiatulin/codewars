-- The Tale of Two Zeros
-- http://www.codewars.com/kata/563c39f92f7bed9572000011/

module Codewars.Kata.NegativeZero where

import Prelude hiding (isNegativeZero)

isNegativeZero :: Double -> Bool
isNegativeZero x = 1/x == 1/(-0.0)
