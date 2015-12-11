-- Sum of the first nth term of Series
-- http://www.codewars.com/kata/555eded1ad94b00403000071

module Codewars.Kata.NthSeries where

import Text.Printf (printf)

seriesSum :: Integer -> String
seriesSum 0 = "0.00"
seriesSum n = printf "%.2f" (sum $ map (1.0/) (take (fromIntegral n) [1, 4 ..]) :: Double)
