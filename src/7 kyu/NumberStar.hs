-- Number-Star ladder
-- https://www.codewars.com/kata/5631213916d70a0979000066

module Codewars.Kata.NumberStar where

import Data.List(intercalate)

pattern :: Int -> String
pattern n = intercalate "\n" . ("1" :) . map (\i -> "1" ++ replicate (pred i) '*' ++ show i) $ [2..n]
