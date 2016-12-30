-- Digits explosion
-- https://www.codewars.com/kata/585b1fafe08bae9988000314

module Kata (explode) where

import Data.Char (digitToInt)

explode :: String -> String
explode = concatMap (\c -> (`replicate` c) . digitToInt $ c)
