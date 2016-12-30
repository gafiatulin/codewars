-- Decode the Morse code
-- https://www.codewars.com/kata/54b724efac3d5402db00065e

module Codewars.Kata.DecodeMorse (decodeMorse) where

import Codewars.Kata.DecodeMorse.Preload (morseCodes)

import Data.List.Split (splitOn)
import Data.Map.Strict ((!))

decodeMorse :: String -> String
decodeMorse = unwords . filter (not . null) . map (concatMap (morseCodes!) . words) . splitOn "   "
