-- That's mean
-- http://www.codewars.com/kata/54b709b811ac249fdb000296

module Mean where

mean :: (Integral a, Fractional b) => [a] -> b
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)
