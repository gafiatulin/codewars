-- Filter the number
-- http://www.codewars.com/kata/55b051fac50a3292a9000025

module Codewars.Kata.Filter where

import Data.Char (isNumber)

filterString :: String -> Int
filterString =  read . filter isNumber 
