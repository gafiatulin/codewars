-- Points On A Line
-- http://www.codewars.com/kata/53b7bc844db8fde50800020a/

module PointsOnALine where

type Point = (Rational, Rational)

onLine :: [Point] -> Bool
onLine ps = if length unique <= 2
            then True
            else all (\p -> (fst p1 * (snd p2 - snd p) + fst p2 * (snd p - snd p1) + fst p * (snd p1 - snd p2)) == 0) (drop 2 ps)
    where f [] = []
          f (x: xs) = x : f (filter (x/=) xs)
          unique = f ps
          p1 = head unique
          p2 = head . tail $ unique
