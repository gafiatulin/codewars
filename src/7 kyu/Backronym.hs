-- makeBackronym
-- http://www.codewars.com/kata/55805ab490c73741b7000064/

module Codewars.Exercise.Backronym where

import Codewars.Exercise.Backronym.Dictionary (dict)
import Data.Char (toUpper)

makeBackronym :: String -> String
makeBackronym = unwords . map (dict . toUpper)
