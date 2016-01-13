-- Simpson's Rule - Approximate Integration
-- http://www.codewars.com/kata/565abd876ed46506d600000d/

module Codewars.G964.Simpson (simpson) where

simpson :: Int -> Double
simpson = sIntegrate ((*1.5) . (**3) . sin) 0 pi 

sIntegrate f a b n = (* (h/3)) . (+ f a ) . ( + f b) . sum . map g $ [1 .. pred n]
    where h = (b - a) / fromIntegral n
          g x  = (if odd x then (*4) else (*2)) . f . (+a) . (*h) . fromIntegral $ x
