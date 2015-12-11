-- Formatting decimal places #1
-- http://www.codewars.com/kata/5641c3f809bf31f008000042

module Codewars.Kata.Decimals where

import Text.Printf (printf)

twoDecimalPlaces :: Double -> Double
twoDecimalPlaces = read 
                 . (\ (f, s) -> f ++ take 3 s) 
                 . break (== '.') 
                 . printf "%f"
