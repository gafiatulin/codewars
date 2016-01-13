-- Anagram or not
-- http://www.codewars.com/kata/546274b0eaa31f79c9000d5d/

module Codewars.Kata.Anagram where

import Data.Char (isAlphaNum, toLower)
import Data.List (sort)
import Control.Arrow ((***))

isAnagramOf :: String -> String -> Bool
isAnagramOf = curry (uncurry (==) . (f *** f))
    where f = sort . filter isAlphaNum . map toLower
