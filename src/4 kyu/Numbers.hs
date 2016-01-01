-- Catching Car Mileage Numbers
-- http://www.codewars.com/kata/52c4dd683bfd3b434c000292/

module Awesome.Numbers where

import Data.Tuple (swap)
import Data.List (unfoldr, isInfixOf)
import Control.Applicative (liftA2)

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs | g x = Yes
                   | g (x+1) || g (x+2) = Almost
                   | otherwise = No
                   where f 0 = Nothing
                         f x = Just . swap . (`divMod` 10) $ x
                         z h = (`isInfixOf` h) . reverse . unfoldr f
                         g x | x < 100  = False
                             | all (=='0') . tail . show $ x = True
                             | (\str -> all (== head str) . tail $ str) . show $ x = True
                             | liftA2 (==) id reverse . show $ x = True
                             | z ([1..9] ++ [0]) x = True
                             | z [9,8..0] x = True
                             | x `elem` xs = True
                             | otherwise = False
