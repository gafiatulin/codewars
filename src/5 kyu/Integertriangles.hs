-- Integer triangles
-- https://www.codewars.com/kata/55db7b239a11ac71d600009d

module Codewars.G964.Integertriangles where

import qualified Data.Vector as Vector (generate, (!))
import Data.IntMap.Strict (fromAscList, member, (!))

squares = fromAscList . map (\x -> (x*x, x)) $ [1..100000]
precomputed = Vector.generate 100000 f
    where f per = length [1 | b <- [1 .. per `div` 2], c <- [1 .. b], let a = b*b + c*c + b*c in member a squares && (squares ! a) + b + c <= per]

giveTriang :: Int -> Int
giveTriang per = (Vector.!) precomputed per
