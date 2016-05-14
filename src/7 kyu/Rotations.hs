-- All Inclusive?
-- http://www.codewars.com/kata/5700c9acc1555755be00027e

module Codewars.G964.Rotations where

import Data.List (inits, tails)

containAllRots :: String -> [String] -> Bool
containAllRots s arr = all (`elem` arr) . init . zipWith (++) (tails s) . inits $ s