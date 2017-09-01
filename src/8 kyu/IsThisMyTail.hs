-- Is this my tail?
-- https://www.codewars.com/kata/56f695399400f5d9ef000af5

module Codewars.IsThisMyTail where

import Data.Maybe(listToMaybe)

correctTail :: String -> Char -> Bool
correctTail b t = maybe False (== t) . listToMaybe . reverse $ b
