-- Moves in squared strings (II)
-- https://www.codewars.com/kata/56dbe7f113c2f63570000b86

module Codewars.G964.Opstrings2 where

import Data.List (intercalate)

rot :: String -> String
rot = intercalate "\n" . reverse . map reverse . lines

selfieAndRot str = intercalate "\n" (f (++ dots) str ++ f (dots ++) (rot str))
    where f g = map g . lines
          dots = replicate n '.'
          n = length . head . lines $ str

oper :: (String -> String) -> String -> String
oper fct strng = fct $ strng
