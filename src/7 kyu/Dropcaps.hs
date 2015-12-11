-- Dropcaps
-- http://www.codewars.com/kata/559e5b717dd758a3eb00005a/

module Codewars.Exercise.Dropcaps where

import Data.List (unfoldr, span)
import Data.Char (isLetter, isSpace, toUpper, toLower)

dropCap = concatMap (\w   -> if (length w > 2) && isLetter (head  w) 
                             then (toUpper . head) w  : map toLower (tail w)
                             else w)
        . unfoldr   (\str -> if null str
                             then Nothing 
                             else Just (span (if isLetter (head str) then isLetter else not . isLetter ) str ))
