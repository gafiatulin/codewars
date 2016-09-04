-- ToLeetSpeak
-- https://www.codewars.com/kata/57c1ab3949324c321600013f

module LeetSpeak(toLeetSpeak) where

import Data.Maybe (fromMaybe)

dict = [ ('A', '@'), ('B' , '8'), ('C' , '('), ('E' , '3'), ('G' , '6'), ('H', '#'), ('I' , '!'), ('L' , '1'), ('O', '0'), ('S' , '$'), ('T' , '7'), ('Z' , '2')]
toLeetSpeak :: String -> String
toLeetSpeak = map (\x -> fromMaybe x . lookup x $ dict)
