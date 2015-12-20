-- Convert string to camel case
-- http://www.codewars.com/kata/517abf86da9663f1d2000003/

module CamelCase where

import Data.Char (toUpper)
import Data.List.Split (split, dropDelims, oneOf)

toCamelCase :: String -> String
toCamelCase str = f . split (dropDelims $ oneOf "-_") $ str
    where f [] = []
          f [x] = x
          f (x:xs) = x ++ concatMap g xs
          g [] = []
          g (x:xs) = toUpper x : xs
