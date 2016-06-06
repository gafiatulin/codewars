-- Salesman's Travel
-- http://www.codewars.com/kata/56af1a20509ce5b9b000001e

module Codewars.G964.Salesmantravel where

import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOn)

travel :: String -> String -> String
travel r zipcode = zipcode ++ ":" ++ cityStreat ss ++ "/" ++ houses ss
    where ss | (== 8) . length $ zipcode = filter (zipcode `isInfixOf`) . splitOn "," $ r
             | otherwise = []
          cityStreat [] = []
          cityStreat xs = intercalate "," . map ( unwords . init . init . tail . words) $ xs
          houses [] = []
          houses xs  = intercalate "," . map (head . words) $ xs
