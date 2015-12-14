-- Word a9n (abbreviation)
-- http://www.codewars.com/kata/5375f921003bf62192000746/

module A9n where

import Data.Char (isLetter)
import Data.List (groupBy)
import Data.Function (on)

abbreviate :: String -> String
abbreviate = concatMap (\w -> if isLetter . head $ w then f w else w) . groupBy ((==) `on` isLetter)
    where f x | length x < 4 = x
              | otherwise = [head x] ++ show (length x -2) ++ [last x]
