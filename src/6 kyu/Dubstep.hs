-- Dubstep
-- http://www.codewars.com/kata/551dc350bf4e526099000ae5/

module Codewars.Kata.Dubstep where

import Data.List.Split (split, dropDelims, dropBlanks, onSublist)

songDecoder :: String -> String
songDecoder = unwords . split (dropDelims . dropBlanks $ onSublist "WUB")
