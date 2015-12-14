-- Reverse words
-- http://www.codewars.com/kata/5259b20d6021e9e14c0010d4/

module Reverse where

import Data.List (unfoldr)

reverseWords :: String -> String
reverseWords = concatMap reverse . unfoldr (\rem -> if null rem then Nothing else Just . span (if (==' ') . head $ rem then (==' ') else not . (==' ')) $ rem)
