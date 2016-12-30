-- Hungarian Vowel Harmony (easy)
-- https://www.codewars.com/kata/57fd696e26b06857eb0011e7

module Kata (dative) where

import Data.List (find)
import Data.Maybe (fromMaybe)

fvs = "eéiíöőüű"
bvs = "aáoóuú"

dative :: String -> String
dative s | fromMaybe False . fmap (`elem` fvs) $ (lastVowel s) = s ++ "nek"
         | fromMaybe False . fmap (`elem` bvs) $ (lastVowel s) = s ++ "nak"
         | otherwise = s
         where lastVowel = find (`elem` (fvs ++ bvs)) . reverse
