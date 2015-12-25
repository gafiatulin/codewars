-- Second Variation on Caesar Cipher
-- http://www.codewars.com/kata/55084d3898b323f0aa000546/

module Codewars.Kata.Caesar2 (encodeStr, decode) where

import Data.Char (chr, ord, toLower)
import Data.List.Split (chunksOf)

encodeStr :: String -> Int -> [String]
encodeStr str s = chunksOf (ceiling . (/5) . (+2) . fromIntegral . length $ str) (prefix ++ map (f s) str)
    where prefix = (\c -> [c, f s c]) . toLower . head $ str

decode :: [String] -> String
decode xs = map (f shift) str 
    where ([c1, c2], str) = splitAt 2 . concat $ xs
          shift = if c2 > c1 then ord c1 - ord c2 else ord c1 - ord c2 - 26

f s c | c `elem` ['a'..'z'] = shift c 'a' s
      | c `elem` ['A'..'Z'] = shift c 'A' s
      | otherwise = c
      where shift c a s = chr . (ord a +) . (`mod` 26) $ (ord c - ord a + s)
