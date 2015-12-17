-- Parabolic Arc Length
-- http://www.codewars.com/kata/562e274ceca15ca6e70000d3/

module Codewars.G964.Arcparab where

import Numeric (showFFloat)

lenCurve :: Integer -> Double

lenCurve n = truncate9 . sum . zipWith (\(x1, y1) (x2, y2) -> sqrt ((x2-x1)^2 + (y2-y1)^2)) ps $ tail ps
    where f x = x * x
          h = 1 / fromIntegral n
          xs = take (fromIntegral n+1) . iterate (+ h) $ 0
          ys = map f xs
          ps = zip xs ys
          truncate9 x = read (showFFloat (Just 9) x "")
