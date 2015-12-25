-- Easy wallpaper
-- http://www.codewars.com/kata/567501aec64b81e252000003/

module Codewars.G964.Wallpaper where

wallpaper :: Double -> Double -> Double -> String
wallpaper l w h | elem 0 [l, w, h] = "zero"
                | otherwise = (nums !!) . floor $ 2.30 * ( l + w ) * h / 5.2
                where nums = [ "one", "two", "three", "four", "five"
                             , "six", "seven", "eight", "nine", "ten"
                             , "eleven", "twelve", "thirteen", "fourteen", "fifteen"
                             , "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]
