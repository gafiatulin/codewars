-- Detect Pangram
-- http://www.codewars.com/kata/545cedaa9943f7fe7b000048/

module Pangram where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram str = all (`elem` map toLower str) ['a'..'z']
