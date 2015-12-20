-- Bowling score calculator
-- http://www.codewars.com/users/gafiatulin

module Bowling where

score rolls total 10 = total
score [x,y] total frame = total + x + y
score (x:y:z:rest) total frame 
    | x == 10     = score (y:z:rest) (total + x + y + z) (frame + 1)
    | x + y == 10 = score (z:rest)   (total + x + y + z) (frame + 1)
    | otherwise   = score (z:rest)   (total + x + y)     (frame + 1)

bowlingScore :: [Int] -> Int
bowlingScore rolls = score rolls 0 0
