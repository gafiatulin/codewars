-- Conway's Game of Life - Unlimited Edition
-- http://www.codewars.com/kata/52423db9add6f6fc39000354/

module UnlimitedGameOfLife (getGeneration) where

import UnlimitedGameOfLife.Preloaded (htmlize)
import Data.List (sort, group)
import Data.Function (on)
import Control.Arrow ((&&&), (***))

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration world n | n < 1 = world
                      | otherwise = g . life (f world) $ n
life world 0 = world
life world n = life (step world) (pred n)
step cells = [head g | g <- h cells, viable g]
    where h = group . sort . concatMap neighbors
          neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]
          viable [_,_,_] = True
          viable [c,_] = c `elem` cells
          viable _ = False
f xs = [(i, j) | (i, xs') <- zip [0..] xs, (j, x) <- zip [0..] xs', x>0]
g [] = [[]]
g alive = [ map (\j -> if (i, j) `elem` alive then 1 else 0) [minJ..maxJ] | i <- [minI..maxI]]
    where ((minI, maxI), (minJ, maxJ)) = ((minimum &&& maximum) *** (minimum &&& maximum)) . unzip $ alive
