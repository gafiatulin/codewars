-- Thinkful - List and Loop Drills: Lists of lists
-- https://www.codewars.com/kata/586e1d458cb711f0a800033b

module Kata (processData) where

processData :: [[Int]] -> Int
processData = product . map (foldl1 (-))
