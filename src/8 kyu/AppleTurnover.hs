-- Alan Partridge II - Apple Turnover
-- https://www.codewars.com/kata/580a094553bd9ec5d800007d

module Codewars.AlanPartridge.AppleTurnover where

apple :: Either String Int -> String
apple = (\x -> if x > 1000 then "It's hotter than the sun!!" else "Help yourself to a honeycomb Yorkie for the glovebox.") . (^2) . either read id
