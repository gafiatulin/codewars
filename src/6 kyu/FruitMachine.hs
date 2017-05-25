-- Fruit Machine
-- https://www.codewars.com/kata/590adadea658017d90000039

module Haskell.SylarDoom.FruitMachine (fruit) where

import Data.List (elemIndex, sortBy, group)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Control.Arrow ((&&&))

fruit :: [[String]] -> [Int] -> Int
fruit reels = f . sortBy (flip compare `on` fst) . map (length &&& head) . group . sortBy (compare `on` id) . mapMaybe (\(r, i) -> fmap succ . (`elemIndex` fruits) $ (reels !! r !! i)) . zip [0..]
    where fruits = ["Jack","Queen","King","Bar","Cherry","Seven","Shell","Bell","Star","Wild"]
          f l | (== 1) . length $ l = (*10) . snd . head $ l
              | (== 2) . length $ l = (* (if (== 10) . snd . last $ l then 2 else 1)) . snd . head $ l
              | otherwise = 0
