-- up AND down
-- http://www.codewars.com/kata/56cac350145912e68b0006f0/

module Codewars.G964.Arrangestring where

import Data.Char (toLower, toUpper)

arrange :: String -> String
arrange = unwords . zipWith ($) (cycle [map toLower, map toUpper]) . f LT . words
    where h LT = GT
          h GT = LT
          f _ [] = []
          f _ [x] = [x]
          f g (x:y:xs) | h g == compare (length x) (length y) = y : f (h g) (x : xs)
                       | otherwise = x : f (h g) (y : xs)
