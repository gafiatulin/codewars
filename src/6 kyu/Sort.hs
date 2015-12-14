-- Sort Arrays (Ignoring Case)
-- http://www.codewars.com/kata/51f41fe7e8f176e70d0002b9/

module Sort where

import Data.Char (toLower)
import Data.List (sortBy)
import Data.Function (on)

sortme :: [String] -> [String]
sortme = sortBy (compare `on` map toLower)
