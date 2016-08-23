-- Star Triangle
-- https://www.codewars.com/kata/57a0f354e298a77602000159

module StarTriangle where

makeStarTriangle :: Int -> String
makeStarTriangle n = init . unlines . map (\x -> replicate (n-x) ' ' ++ replicate (2*(x-1) + 1) '*') $ [1..n]
