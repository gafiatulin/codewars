-- Maze Runner
-- https://www.codewars.com/kata/58663693b359c4a6560001d6

module Haskell.SylarDoom.MazeRunner where

import qualified Data.Map.Strict as Map
import Data.List (find)

mazeRunner :: [[Int]] -> String -> String
mazeRunner maze = run start . filter (`elem` "NEWS")
    where df = [('N', \(x, y) -> (x, y - 1)), ('E', \(x, y) -> (x + 1, y)), ('S', \(x, y) -> (x, y + 1)), ('W', \(x, y) -> (x - 1, y))]
          m = Map.fromList . concatMap (\(i, s) -> zipWith (\j c -> ((j, i), c)) [0..] s) . zip [0..] $ maze
          start = fmap fst . find ((== 2) . snd) . Map.toList $ m
          run :: Maybe (Int, Int) -> String -> String
          run _ [] = "Lost"
          run position (x:xs) = case (\op -> (op >>= (`Map.lookup` m), op)) (lookup x df >>= (`fmap` position)) of
              (Just 1, _) -> "Dead"
              (Just 3, _) -> "Finish"
              (Just _, p) -> run p xs
              (Nothing, _) -> "Dead"
