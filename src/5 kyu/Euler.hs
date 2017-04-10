-- Euler's method for a first-order ODE
-- http://www.codewars.com/kata/56347fcfd086de8f11000014/

module Codewars.G964.Euler where

import Control.Arrow ((&&&))

exEuler :: Fractional a => Int -> a
exEuler n = (/10^6) . fromIntegral . truncate . (*10^6) $ se / (succ . fromIntegral $ n)
    where h  = (1/) . fromIntegral $ n
          se :: Double
          se = sum . map abs . zipWith (flip (/)) z . zipWith (-) y $ z
          y = take (n+1) . map fst . iterate (\(y, x) -> (y + h * (2 - exp ((-4)*x) - 2*y) , x+h)) $ (1, 0)
          z = map (succ . (*0.5) . uncurry (-) . (exp . (* (-4)) &&& exp . (* (-2))) . (*h) . fromIntegral) [0..n]
