-- A small difference
-- https://www.codewars.com/kata/580b746830f829e46400001e

module ASmallDifference where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (isPrefixOf, inits)

aSmallDifference :: String -> String -> Bool
aSmallDifference s1 s2 | length s1 == length s2 = (== 1) . length . filter not . zipWith (==) s1 $ s2
                       | (length s1) - (length s2) == 1 = f s1 s2
                       | (length s2) - (length s1) == 1 = f s2 s1
                       | otherwise = False
                       where f a b = let p = prefix a b in (== b) . (p ++) . drop (succ . length $ p) $ a
                             prefix a b = fromMaybe "" . listToMaybe . reverse . takeWhile (`isPrefixOf` b) . inits $ a
