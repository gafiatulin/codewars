-- Rainfall
-- http://www.codewars.com/kata/56a32dd6e4f4748cc3000006/

module Codewars.G964.Rainfall (mean, variance) where

import Data.List (isPrefixOf, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Control.Arrow ((&&&))

mean :: String -> String -> Double
mean town = fromMaybe (-1) . liftM (uncurry (/) . (sum &&& fromIntegral . length)) . f town

variance :: String -> String -> Double
variance town = fromMaybe (-1) . (\(a, mxs) -> liftM (\xs ->  uncurry (/) . (sum . map ((^2) . (a -)) &&& fromIntegral . length) $ xs) mxs) . (mean town &&& f town)

f :: String -> String -> Maybe [Double]
f town = liftM (map read . splitOn "," . filter (`elem` "0123456789.,")) . find (isPrefixOf town) . lines
