-- Checkerboard Generation
-- http://www.codewars.com/kata/53dc08fa8a0c93229400023b/

module Checkerboard where

import Data.List ( unfoldr)

checkerboard n | n <= 0 = ""
               | otherwise = unlines . unfoldr (\rest-> if null rest then Nothing else Just . splitAt (3*n) $ rest) . pattern $ n
               where pattern n | odd n = take (3 * n * n) . cycle $ "[r][b]"
                               | otherwise = [row | i <-  take n . cycle $ [0,1] , row <- take (3*n) . drop (3*i) . cycle $ "[r][b]"]
