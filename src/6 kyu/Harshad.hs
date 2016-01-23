-- Harshad or Niven numbers
-- http://www.codewars.com/kata/54a0689443ab7271a90000c6/

module Codewars.Kata.Harshad where

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

isValid  :: Integer -> Bool
isValid = (==0) . uncurry mod . (id &&& fromIntegral . foldr ((+) . digitToInt) 0 . show)

getNext  :: Integer -> Integer
getNext n = head . filter isValid $ [n+1, n+2 ..]

getSerie :: Int -> Maybe Integer -> [Integer]
getSerie n = take n . iterate getNext . getNext . fromMaybe 0
