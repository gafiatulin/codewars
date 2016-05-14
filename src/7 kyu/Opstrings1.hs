-- Moves in squared strings (I)
-- http://www.codewars.com/kata/56dbe0e313c2f63be4000b25

module Codewars.G964.Opstrings1 (oper, vertMirror, horMirror) where

vertMirror :: String -> String
vertMirror = init . unlines . map reverse . lines

horMirror :: String -> String
horMirror = init . unlines . reverse . lines

oper :: (String -> String) -> String -> String
oper = ($)
