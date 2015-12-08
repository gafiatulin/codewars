-- Isograms
-- http://www.codewars.com/kata/54ba84be607a92aa900000f1

module Isogram where

import Data.Char (toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram s = length (nub str) == length (str) where str = map toLower s
