-- All unique
-- http://www.codewars.com/kata/553e8b195b853c6db4000048/

module Codewars.Kata.Unique where

import Data.List (sort, group)

hasUniqueChar :: String -> Bool
hasUniqueChar =  not . any ((> 1) . length) . group . sort
