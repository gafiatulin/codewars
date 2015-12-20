module Codewars.Exercise.Tongues where

import Data.Char (toLower, isLower, ord, chr)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

tongues :: String -> String
tongues = map f 
    where vowels = ("aiyeou", 3)
          consonants = ("bkxznhdcwgpvjqtsrlmf", 10)
          f c | toLower c `elem` fst vowels = decypher vowels c
              | toLower c `elem` fst consonants = decypher consonants c
              | otherwise = c
          decypher (charSequence , shift) c = 
              let i = (`mod` length charSequence) . (+ shift) . fromJust . elemIndex (toLower c) $ charSequence
              in chr . (+ ord (if isLower c then 'a' else 'A')) 
                     . (+ (negate . ord $ 'a')) 
                     . ord . (!! i) $ charSequence
