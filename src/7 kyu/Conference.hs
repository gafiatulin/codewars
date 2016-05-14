-- Conference Traveller
-- http://www.codewars.com/kata/56f5594a575d7d3c0e000ea0

module Codewars.Kata.Conference where

import Data.List(find)

conferencePicker :: [String] -> [String] -> Maybe String
conferencePicker visited = find (`notElem` visited)