-- Tarot Score
-- https://www.codewars.com/kata/5868022b36959fa4a4000527

module Codewars.Kata.TarotScore where
import Data.List (isInfixOf, elemIndex, genericLength)
import Data.Maybe (mapMaybe)

tarotScore :: String -> Rational
tarotScore s = sum . map (\f -> f cards) $ [(* 0.5) . genericLength, sum . mapMaybe (fmap fromIntegral . (`elemIndex` " JCQK") . last), (*4) . genericLength . f, negate . ([ 56, 51, 41, 36 ] !!) . genericLength . f]
    where cards = split s
          split [] = []
          split xs = take 2 xs : split (drop 2 xs)
          f = filter (`elem` ["00", "01", "21"])
