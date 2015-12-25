-- Get the Middle Character
-- http://www.codewars.com/kata/56747fd5cb988479af000028/

module Codewars.G964.Getmiddle where

getMiddle :: String -> String
getMiddle "" = ""
getMiddle s | odd . length $ s =  [(s !!) . (`div` 2) . length $ s]
            | otherwise = (\i -> [s !! pred i, s !! i]) . (`div` 2) . length $ s
