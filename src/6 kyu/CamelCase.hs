-- CamelCase Method
-- https://www.codewars.com/kata/587731fda577b3d1b0001196

module CamelCase.JorgeVS.Kata where

import Data.Char (toUpper)

camelCase :: String -> String
camelCase = concatMap (\(x:xs) -> toUpper x:xs) . words
