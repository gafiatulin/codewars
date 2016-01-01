-- Counting Change Combinations
-- http://www.codewars.com/kata/541af676b589989aed0009e7/

module Change where

import Data.List (sortBy)

countChange :: Integer -> [Integer] -> Integer
countChange n ss  = f n (sortBy (flip compare) ss)
    where f 0 _ = 1
          f _ [] = 0
          f n [x] = toInteger . fromEnum . (==0) . mod n $ x
          f n (x:xs) = sum . map (\d -> f (n-d) xs) . takeWhile (<=n) $ (0:[x, 2*x..])