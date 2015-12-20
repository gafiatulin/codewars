-- Break camelCase
-- http://www.codewars.com/kata/5208f99aee097e6552000148/

module Codewars.Kata.BreakCamelCase where

import Data.Char (isUpper)
import Data.List (unfoldr)
import Control.Arrow (first)

solution :: String -> String
solution = unwords . unfoldr f
    where f [] = Nothing
          f (x:xs) = Just . first ((:) x) . break isUpper $ xs
