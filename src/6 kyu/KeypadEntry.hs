-- Multi-tap Keypad Text Entry on an Old Mobile Phone
-- http://www.codewars.com/kata/54a2e93b22d236498400134b/

module Haskell.Codewars.KeypadEntry where

import Data.Char (toUpper)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

presses :: String -> Int
presses = sum . map ((\ c -> (+ 1) . fromJust . elemIndex c . head . filter (\ b -> c `elem` b) $ bs) . toUpper)
    where bs = ["1", "ABC2", "DEF3", "GHI4", "JKL5", "MNO6", "PQRS7", "TUV8", "WXYZ9", "*", " 0", "#"]
