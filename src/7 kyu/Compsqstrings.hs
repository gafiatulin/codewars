-- Composing squared strings
-- http://www.codewars.com/kata/56f253dd75e340ff670002ac

module Codewars.G964.Compsqstrings where

import Data.List (inits)
import Data.Tuple (swap)

compose :: String -> String -> String
compose s1 = init . unlines . zipWith (++) (f s1) . g
    where f = map (uncurry (!!) . swap) . zip [0 ..] . map (tail . inits) . lines
          g = map (uncurry take) . reverse . zip [1..] . lines
