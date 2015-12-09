-- Sum up the random string
-- http://www.codewars.com/kata/55da6c52a94744b379000036/

module Codewars.Kata.SumString where

import Data.List (unfoldr, span)
import Data.Char (isDigit)

sumFromString = sum
              . map read
              . filter (not. null) 
              . unfoldr (\str -> if null str then Nothing else Just (if isDigit (head str) then span isDigit str else  ("" , tail str)) )
