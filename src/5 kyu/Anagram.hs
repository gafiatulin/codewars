-- Where my anagrams at?
-- http://www.codewars.com/kata/523a86aa4230ebb5420001e1/

module Anagram where

import Data.List (sort)

anagrams :: String -> [String] -> [String]
anagrams w = filter $ (sort w ==) . sort
