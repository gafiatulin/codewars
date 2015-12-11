-- Two to One
-- http://www.codewars.com/kata/5656b6906de340bd1b0000ac/

module Codewars.G964.Longest where

import Data.List (sort, group)

longest :: [Char] -> [Char] -> [Char]
longest = (\s1 s2 ->  map head . group . sort $ s1++s2)
