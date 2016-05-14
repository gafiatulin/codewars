-- Beginner Series #5 Triangular Numbers
-- http://www.codewars.com/kata/56d0a591c6c8b466ca00118b/

module Codewars.Numbers where

import Control.Arrow ((&&&))

isTriangular :: Int -> Bool
isTriangular = uncurry (==) . (id &&& (^2) . round . sqrt . fromIntegral) . succ . (*8)
