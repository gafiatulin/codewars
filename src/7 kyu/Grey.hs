-- 254 shades of grey
-- http://www.codewars.com/kata/54d22119beeaaaf663000024/

module Codewars.Kata.Grey where

import Numeric (showHex)

shadesOfGrey :: Int -> [String]
shadesOfGrey n | n < 0 = []
               | n >= 255 = shadesOfGrey 254
               | otherwise =  map (\x -> '#' : (take 6 . cycle . intToHexString $ x)) [1..n]
               where intToHexString n | n < 16 = "0" ++ showHex n ""
                                      | otherwise = showHex n ""
