-- Are we alternate?
-- https://www.codewars.com/kata/59325dc15dbb44b2440000af

module Kata.AreWeAlternate (isAlt) where

import Data.List (group)
import Data.Char (toLower)

isAlt :: String -> Bool
isAlt = all ((==1) . length) . group . map ((`elem` "aeiou") . toLower)
