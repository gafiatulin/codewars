-- HTML Complementary Color
-- http://www.codewars.com/kata/56be4affc5dc03b84b001d2d

module Codewars.Kata.HTMLComplementaryColor where

import Data.Char (isHexDigit, toUpper)
import Numeric (showHex, readHex)

getReversedColor :: String -> Maybe String
getReversedColor [] = Just "#FFFFFF"
getReversedColor xs | ((<= 6) . length $ xs) && all isHexDigit xs = Just . ('#':) . map toUpper . showHex ((0XFFFFFF - ) . fst . head . readHex $ xs) $ ""
                    | otherwise = Nothing
