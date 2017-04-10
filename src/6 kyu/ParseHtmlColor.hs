-- Parse HTML/CSS Colors
-- http://www.codewars.com/kata/58b57ae2724e3c63df000006

module Codewars.Lambda4fun.ParseHtmlColor (parseHtmlColor) where

import Codewars.Lambda4fun.ParseHtmlColor.PresetColors (presetColors)
import Data.Map.Strict (Map, fromList, (!))
import Data.List.Split (chunksOf)
import Data.Char (toLower)
import Numeric (readHex)

parseHtmlColor :: String -> Map Char Int
parseHtmlColor ('#':xs) | length xs == 3 = parseHtmlColor . ('#':) . concatMap (\c -> [c, c]) $ xs
                        | otherwise = fromList . zip "rgb" . map (fst . head . readHex) . chunksOf 2 $ xs
parseHtmlColor s = parseHtmlColor . (presetColors !) . map toLower $ s
