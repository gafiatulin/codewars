-- noobCode 01: SUPERSIZE ME.... or rather, this integer!
-- http://www.codewars.com/kata/5709bdd2f088096786000008

module Codewars.Kata.Supersize where
import Data.List (sortBy)

superSize :: Integer -> Integer
superSize = read . sortBy (flip compare) . show