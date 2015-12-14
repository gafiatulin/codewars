-- Title Case
-- http://www.codewars.com/kata/5202ef17a402dd033c000009/

module (titleCase) where

import Data.Char (toLower, toUpper)

titleCase :: String -> String -> String
titleCase _ "" = ""
titleCase minor title = unwords ((capitalise . head $ tw) : map (\w -> if map toLower w `elem` mw then map toLower w else capitalise w) (tail tw))
    where tw = words title
          mw = map (map toLower) (words minor)
          capitalise w = toUpper (head w) : map toLower (tail w)
