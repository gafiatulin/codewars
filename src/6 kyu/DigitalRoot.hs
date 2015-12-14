-- Sum of Digits / Digital Root
-- http://www.codewars.com/kata/541c8630095125aba6000c00/

module DigitalRoot where

import Data.Char (digitToInt)

digitalRoot :: Integral a => a -> a
digitalRoot n | (n `div` 10) == 0 = n
              | otherwise = digitalRoot . fromIntegral . sum . map digitToInt . show . fromIntegral $ n
