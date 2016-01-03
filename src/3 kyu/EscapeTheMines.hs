-- Escape the Mines !
-- http://www.codewars.com/kata/5326ef17b7320ee2e00001df/

module EscapeTheMines where

import Data.List (delete)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Control.Arrow ((&&&), first, second)

type XY = (Int,Int)
data Move = U | D | R | L deriving (Eq, Show)

solve :: [[Bool]] -> XY -> XY -> [Move]
solve grid start = reverse . fromJust . path [U, D, R, L]
    where neg = fromJust . flip lookup [(U, D), (D, U), (R, L), (L, R)]
          move = fromJust . flip lookup [(U, second pred), (D, second succ), (R, first succ), (L, first pred)]
          (lenX , lenY) = (length &&& length . head) grid
          inGrid (x, y) = 0 <= x && x < lenX && 0 <= y && y < lenY && grid !! x !! y
          path moves finish | start == finish = Just []
                            | not . inGrid $ finish = Nothing
                            | otherwise = listToMaybe . mapMaybe (\m -> fmap (neg m:) . path (delete (neg m) [U, D, R, L]) $ (move m finish)) $ moves
