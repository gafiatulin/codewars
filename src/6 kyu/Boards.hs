-- Checkered Board
-- http://www.codewars.com/kata/5650f1a6075b3284120000c0/

module Codewars.Boards where

import Data.List (unfoldr)

checkeredBoard :: Int -> Maybe String
checkeredBoard n | n < 2 = Nothing
                 | otherwise =  Just . init . unlines . unfoldr(\rest -> if null rest then Nothing else Just (take (2*n-1) rest, drop (2*n) rest) ) . pattern $ n
                 where pattern n | odd n = take (2*n*n) . cycle $ "■ □ "
                                 | otherwise =  [row | i <-  take n . cycle $ [0,1], row <- take (2*n) . drop (2*i) . cycle $ "□ ■ "]
