-- Are You Playing Banjo?
-- http://www.codewars.com/kata/53af2b8861023f1d88000832

module Banjo where

areYouPlayingBanjo :: String -> String
areYouPlayingBanjo name = name ++ if head name == 'R' || head name == 'r' then " plays banjo" else " does not play banjo"
