-- Common Substrings
-- http://www.codewars.com/kata/5669a5113c8ebf16ed00004c/

module Codewars.G964.Commonsubs where

import Control.Arrow ((***))
import Data.Char (toLower)
import Data.List (intersect, inits, tails)

substringTest :: String -> String -> Bool
substringTest s1 = any ((> 1) . length) . uncurry intersect . curry (f *** f) s1
    where f = concatMap inits . tails . map toLower
