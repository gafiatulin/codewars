-- Your order, please
-- https://www.codewars.com/kata/55c45be3b2079eccff00010f

module Codewars.Kata.YourOrderPlease where

import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Ord (comparing)

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy (comparing f) . words
    where f :: String -> Int
          f = read . filter isDigit
