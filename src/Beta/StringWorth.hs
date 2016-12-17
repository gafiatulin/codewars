-- Which string is worth more?
-- https://www.codewars.com/kata/5840586b5225616069000001

module Kata where

import Data.Char (ord)

highestValue :: String -> String -> String
highestValue a b = if f a >= f b then a else b
  where f = sum . map ord
