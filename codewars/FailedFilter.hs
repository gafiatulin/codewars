-- Failed Filter - Bug Fixing #3
-- http://www.codewars.com/kata/55c606e6babfc5b2c500007c/

module FailedFilter (filterNumbers) where

import Data.Char (isDigit)

filterNumbers :: String -> String
filterNumbers = filter (not . isDigit)
