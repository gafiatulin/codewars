-- Strings Mix
-- http://www.codewars.com/kata/5629db57620258aa9d000014

module Codewars.G964.Mixin where

import Data.Map.Lazy (fromList, unionWith, mapWithKey, elems)
import Data.List (group, sort, sortBy, intercalate)
import Data.Char (isLower)

mix :: String -> String -> String
mix s1 s2 = intercalate "/" (sortBy h (elems $ mapWithKey g (unionWith f (fun s1 "1") (fun s2 "2"))))
    where fun str id = fromList 
                     . map (\x -> (head x, (length x, id))) 
                     . filter (\x -> length x > 1) 
                     . group . sort 
                     $ filter isLower str
          f x y = case compare (fst x) (fst y)
                  of LT -> y
                     GT -> x
                     EQ -> (fst x, "=")
          g k v = snd v ++ ":" ++ replicate (fst v) k
          h l r = case compare (length r) (length l) 
                  of EQ -> compare l r
                     x  -> x
