-- Prize Draw
-- http://www.codewars.com/kata/5616868c81a0f281e500005c/

module Codewars.G964.Rank where

import Data.List (sortBy)
import Data.Char (toUpper, ord)
import Data.List.Split (split, dropDelims, oneOf)

rank :: String -> [Int] -> Int -> String
rank "" _ _ = "No participants"
rank st we n = if length pAndR < n then "Not enough participants" else fst . (!!(n-1)) . sortBy cmp $ pAndR
    where pAndR = zipWith (\w name -> (name, w * (length name + (sum . map (\c -> (ord . toUpper $ c) - ord 'A' + 1) $ name)))) we . split (dropDelims $ oneOf ",") $ st
          cmp (name1, rank1) (name2, rank2) = case compare rank2 rank1 of
              EQ -> compare name1 name2
              o1 -> o1
