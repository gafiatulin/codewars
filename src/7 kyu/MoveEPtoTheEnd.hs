-- Exclamation marks series #8: Move all exclamation marks to the end of the sentence
-- https://www.codewars.com/kata/57fafd0ed80daac48800019f

module Kata (remove) where

import Data.List (partition)

remove :: String -> String
remove = uncurry (++) . partition (/= '!')
