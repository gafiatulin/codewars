-- Spy games - rebuild your decoder
-- https://www.codewars.com/kata/586988b82e8d9cdc520003ac

module Kata (decrypt) where

import Data.Maybe (mapMaybe)
import Data.Char (digitToInt, isDigit)

decrypt :: String -> String
decrypt = mapMaybe ( (`lookup` zip [0 ..] (' ' : ['a' .. 'z'])) . (`mod` 27) . sum . map digitToInt . filter isDigit) . words
