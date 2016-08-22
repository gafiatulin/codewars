-- Correct movie title
-- https://www.codewars.com/kata/57a34e2b53ba33994d000668

module MovieTitle where

import Data.Char (toLower, toUpper)

correctMovieTitle :: String -> String
correctMovieTitle = unwords . map f . words
    where f (x:xs) = toUpper x : map toLower xs
