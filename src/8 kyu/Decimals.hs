-- Formatting decimal places #0
-- http://www.codewars.com/kata/5641a03210e973055a00000d

module Codewars.Kata.Decimals where

import Text.Printf (printf)

twoDecimalPlaces :: Double -> Double
twoDecimalPlaces x = read $ printf "%.2f" x
