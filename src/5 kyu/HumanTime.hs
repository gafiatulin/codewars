-- Human Readable Time
-- http://www.codewars.com/kata/52685f7382004e774f0001f7/

module HumanTime where

import Data.List (intercalate)
import Text.Printf (printf)

humanReadable :: Int -> String
humanReadable x = intercalate ":" . map (printf "%02d") $ [x `div` 3600, (x `div` 60) `mod` 60, x `mod` 60]
