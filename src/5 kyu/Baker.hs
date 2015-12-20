-- Pete, the baker
-- http://www.codewars.com/kata/525c65e51bf619685c000059/

module Baker where

import Data.List (sort)

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage | length recipe > length have = 0
                     | otherwise = minimum . zipWith (\(_,h) (_,n) -> h `div` n) (sort have) $ sort recipe
                     where have = filter (\(i, a) -> (i `elem`) . fst . unzip $ recipe ) storage
