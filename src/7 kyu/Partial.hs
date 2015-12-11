-- Partial Word Searching
-- http://www.codewars.com/kata/54b81566cd7f51408300022d/

module Codewars.Kata.Partial where

import Data.Char (toLower)
import Data.List (isInfixOf)

wordSearch :: String -> [String] -> Maybe [String]
wordSearch query seq = if not (null f) then Just f else Nothing
    where f = filter (\str -> map toLower query `isInfixOf` map toLower str) seq
