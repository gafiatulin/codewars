-- Alan Partridge III - London
-- https://www.codewars.com/kata/580a41b6d6df740d6100030c

module Codewars.AlanPartridge.London where

alan :: [String] -> String
alan l = if all (`elem` l) ["Rejection", "Disappointment", "Backstabbing Central", "Shattered Dreams Parkway"] then "Smell my cheese you mother!" else "No, seriously, run. You will miss it."
