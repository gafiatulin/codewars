-- Color Choice
-- http://www.codewars.com/kata/55be10de92aad5ef28000023/

module Codewars.G964.CheckChoose where

import Data.Maybe (listToMaybe, fromMaybe)

checkchoose :: Integer -> Integer -> Integer
checkchoose m n = fromMaybe (-1) . listToMaybe . map snd . filter ((==m) . fst) . map (\x -> (product [x+1..n] `div` product [1..n-x], x)) $ [1..n]
