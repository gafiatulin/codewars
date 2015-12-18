-- Statistics for an Athletic Association
-- http://www.codewars.com/kata/55b3425df71c1201a800009c/

module Codewars.G964.Stat where

import Data.List (sort, intercalate)
import Data.List.Split (split, dropDelims, oneOf)
import Text.Printf (printf)

stat :: String -> String
stat "" = ""
stat results = present . map ((sum . zipWith (*) [3600, 60, 1]) . map (\s -> read s :: Double) . split (dropDelims $ oneOf "|")) . split (dropDelims $ oneOf ",") $ results
    where present xs = "Range: " ++ (formatT . range $ xs) ++ " Average: " ++ (formatT . mean $ xs) ++ " Median: " ++ (formatT . median $ xs)
          range xs = maximum xs - minimum xs
          mean xs = sum xs / (fromIntegral . length $ xs)
          median xs | odd n  = head  $ drop (n `div` 2) xs'
                    | even n = mean $ take 2 $ drop i xs'
                    where i = (length xs' `div` 2) - 1
                          xs' = sort xs
                          n  = length xs
          formatT x = intercalate "|" . map (printf "%02d") $ [h, m, s]
              where t = truncate x :: Int
                    h = t `div` 3600
                    m = (t `div` 60) `mod` 60
                    s = t `mod` 60
