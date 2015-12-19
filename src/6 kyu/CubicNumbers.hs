-- Hidden "Cubic" numbers
-- http://www.codewars.com/kata/55031bba8cba40ada90011c4/

module Codewars.Kata.CubicNumbers where

import Codewars.Kata.CubicNumbers.Types
import Data.Char (isDigit, digitToInt)
import Data.List (unfoldr)

sumOfCubes :: String -> Maybe Lucky
sumOfCubes str = lucky . map (read::String->Int) . filter isCubic . unfoldr f $ str
    where f str = if null str then Nothing else (\(d, r) ->  (\(dd, rr) -> if null dd then Nothing else Just(dd, rr++r)) . span isDigit $ d). splitAt 3. dropWhile (not . isDigit) $ str
          isCubic str = (read str :: Int) == (sum . map ((^3) . digitToInt) $ str)
          lucky xs = if null xs then Nothing else Just $ Lucky xs (sum xs)
