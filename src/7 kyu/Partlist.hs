-- Parts of a list
-- http://www.codewars.com/kata/56f3a1e899b386da78000732

module Codewars.G964.Partlist where

import Data.List (inits, tails)

partlist :: [String] -> [(String, String)]
partlist arr = zip (map unwords . tail . inits $ arr) . map unwords . tail . init . tails $ arr
