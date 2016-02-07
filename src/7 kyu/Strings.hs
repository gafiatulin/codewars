-- Are there doubles?
-- http://www.codewars.com/kata/56a24b4d9f3671584d000039/

module Codewars.Strings where

import Data.Char (toLower)
import Data.List (group)

doubleCheck :: String -> Bool
doubleCheck = any ((>1) . length) . group . map (toLower)
