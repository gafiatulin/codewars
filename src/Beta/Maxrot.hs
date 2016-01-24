-- Rotate for a Max
-- http://www.codewars.com/kata/56a4872cbb65f3a610000026/

module Codewars.G964.Maxrot where

import Data.Char (digitToInt, intToDigit)

maxRot :: Integer -> Integer
maxRot n = maximum . map (read . map intToDigit . fst) . take (length as) . iterate f $ (as, 0)
    where as = map digitToInt . show $ n :: [Int]
          f (xs, n) = ((\(f, s) -> f ++ rotate s) . splitAt n $ xs, succ n)
          rotate [] = []
          rotate [x] = [x]
          rotate (x:xs) = xs ++ [x]
