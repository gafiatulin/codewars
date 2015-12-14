-- Validate Credit Card Number
-- http://www.codewars.com/kata/5418a1dd6d8216e18a0012b2/

module Validate where

import Data.Char (digitToInt)

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sum . map (\n -> if n>9 then n-9 else n) . zipWith (*) (cycle [1, 2]) . reverse . map digitToInt . show
