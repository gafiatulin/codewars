-- Heathrow to London
-- https://www.codewars.com/kata/57fd829e98f76d18fa0004e9

module HeathrowToLondon( Section(..), Label(..), optimalPath) where

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show, Eq)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = reverse (if aprice <= bprice then apath else bpath)
    where (apath, bpath, aprice, bprice) = foldl f ([],[], 0, 0) roadSystem

f :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
f (pathA, pathB, pA, pB) (Section a b c) = (if fp2A <= cp2A then (A,a):pathA else (C,c):(B,b):pathB, if fp2B <= cp2B then (B,b):pathB else (C,c):(A,a):pathA, min fp2A cp2A, min fp2B cp2B)
    where fp2A = pA + a
          cp2A = pB + b + c
          fp2B = pB + b
          cp2B = pA + a + c
