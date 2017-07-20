-- Esolang: MiniBitMove
-- https://www.codewars.com/kata/587c0138110b20624e000253

module Haskell.SylarDoom.MiniBitMove where

import Data.List (foldl')
import Data.Foldable(toList)
import qualified Data.Sequence as Seq

interpreter :: String -> String -> String
interpreter tape array = toList . fst . foldl' (\(a, i) c -> if c == '0' then (a, succ i) else (Seq.adjust flip' i  a, i)) (Seq.fromList array, 0) . take tl . cycle $ tape'
    where tape' = filter (`elem` "01") tape
          tl = (* length tape') . succ . (length array `div`) . length . filter (== '0') $ tape'
          flip' '0' = '1'
          flip' '1' = '0'
          flip' c = c
