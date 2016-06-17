-- Help the fruit guy
-- https://www.codewars.com/kata/557af4c6169ac832300000ba

module Codewars.Kata.Fruits where

import Data.Maybe(fromMaybe)
import Data.Char(toLower)
import Data.List (stripPrefix)

removeRotten :: [String] -> [String]
removeRotten = map ((\ f -> fromMaybe f . stripPrefix "rotten" $ f) . map toLower)
