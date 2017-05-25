-- 8-ball setup validation
-- https://www.codewars.com/kata/58fa711d9fd13e2da9000f8a

module EightBallValid where

import Data.List(reverse, transpose)
import Control.Arrow ((&&&), (***))
import Control.Monad (join)

eightBallValid a = all (\ f -> f a) [eight, bottomCorners, parallel]
    where eight = (== 8) . (!! 1) . (!! 2)
          bottomCorners = uncurry (/=) . (head &&& last) . map isSolid . last
          parallel = uncurry (&&) . (f &&& uncurry (&&) . join (***) (f . transpose) . (id &&& map reverse))
          f = not . any sameColor
          sameColor [_] = False
          sameColor arr = same . map isSolid $ arr
          same xs = and . zipWith (==) xs . tail $ xs
          isSolid = (<= 7)
