-- Manhattan Distance
-- http://www.codewars.com/kata/52998bf8caa22d98b800003a/

module Manhattan where

manhattanDistance :: (Int, Int) -> (Int, Int) -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)
