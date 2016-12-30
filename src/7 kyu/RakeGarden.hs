-- Help Suzuki rake his garden!
-- https://www.codewars.com/kata/571c1e847beb0a8f8900153d

module Kata (rakeGarden) where

rakeGarden :: String -> String
rakeGarden = unwords . map (\s ->  if s `elem` ["rock", "gravel"] then s else "gravel") . words
