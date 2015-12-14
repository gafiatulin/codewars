-- WeIrD StRiNg CaSe
-- http://www.codewars.com/kata/52b757663a95b11b3d00062d/train/haskell

module WeIrDStRiNgCaSe where

import Data.Char(toLower, toUpper)

toWeirdCase :: String -> String
toWeirdCase = unwords . map (zipWith ($) (cycle [toUpper, toLower])) . words
