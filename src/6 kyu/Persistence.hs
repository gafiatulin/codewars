-- Persistent Bugger.
-- http://www.codewars.com/kata/55bf01e5a717a0d57e0000ec/

module Codewars.G.Persistence where

import Data.Char (digitToInt)

persistence :: Int -> Int
persistence n | 10 < n = 0
              | otherwise = (+1) . persistence . product . map digitToInt . show $ n
