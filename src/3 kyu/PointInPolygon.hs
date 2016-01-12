-- Point in Polygon
-- http://www.codewars.com/kata/530265044b7e23379d00076a/

module PointInPolygon where

type Point = (Double, Double)

pointInPoly :: [Point] -> Point -> Bool
pointInPoly poly point = odd . length . filter (intersect (point, 1, 0)) . zip poly $ (tail poly ++ [head poly])
    where intersect ((px, py), dx, dy) ((ax, ay), (bx, by)) = (dx*l /= dy*m) && t1 >= 0 && t2 >= 0 && t2 <= 1
              where t1 = (m*k - n*l)/(dy*m - dx*l)
                    t2 = (dx*k - dy*n)/(dy*m - dx*l)
                    m = bx - ax
                    n = ax - px
                    l = by - ay
                    k = ay - py
