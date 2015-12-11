-- How many lightsabers do you own?
-- http://www.codewars.com/kata/51f9d93b4095e0a7200001b8

module Lightsabers where

howManyLightsabersDoYouOwn :: Num a => [Char] -> a
howManyLightsabersDoYouOwn "Zach" = 18
howManyLightsabersDoYouOwn _ = 0
