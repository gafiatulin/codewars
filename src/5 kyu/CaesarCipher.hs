-- First Variation on Caesar Cipher
-- http://www.codewars.com/kata/5508249a98b3234f420000fb/

module Codewars.Kata.CaesarCipher (demovingShift, movingShift) where

import Data.Char (chr, ord)
import Data.List.Split (chunksOf)
        
demovingShift :: [String] -> Int -> String
demovingShift xs shift = zipWith (curry f) (concat xs) (map negate [shift, shift+1 ..])

movingShift :: String -> Int -> [String]
movingShift s shift = if length strs == 4 then strs ++ [""] else strs
    where strs = chunksOf (ceiling . (/5) . fromIntegral . length $ s) . zipWith (curry f) s $ [shift, shift+1 ..]

f (c, s) | c `elem` ['a'..'z'] = shift c 'a' s
         | c `elem` ['A'..'Z'] = shift c 'A' s
         | otherwise = c
         where shift c a s = chr . (ord a +) . (`mod` 26) $ (ord c - ord a + s)
