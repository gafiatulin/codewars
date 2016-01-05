-- Character Counter
-- http://www.codewars.com/kata/56786a687e9a88d1cf00005d/

module Codewars.G964.Validateword where

import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&))

validateWord :: String -> Bool
validateWord = and . uncurry map . ( (==) . head &&& tail) . map length . group . sort . map toLower
