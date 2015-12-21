-- Sierpinski's Gasket
-- http://www.codewars.com/kata/53ea3ad17b5dfe1946000278/

module Sierpinsky where

import Data.List (intercalate, intersperse)
import Data.Char (isSpace)

sierpinsky :: Integral a => a -> String
sierpinsky n = intercalate "\n" . map (intersperse ' ' . g) . (!! fromIntegral n) . iterate f $ ["L"]
    where f = concatMap (\s -> [concatMap l1 s, concatMap l2 s])
          g = reverse . dropWhile isSpace . reverse
          l1 'L' = "L "
          l1 ' ' = "  " 
          l2 'L' = "LL"
          l2 ' ' = "  "
