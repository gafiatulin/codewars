-- The Hashtag Generator
-- http://www.codewars.com/kata/52449b062fb80683ec000024/

module Codewars.Kata.Hashtag where

import Data.Char (isSpace, toUpper)
import Data.List.Split (wordsBy)

generateHashtag :: String -> Maybe String
generateHashtag = g . concatMap f . wordsBy isSpace
    where f [] = []
          f (x:xs) = toUpper x : xs
          g [] = Nothing
          g s | length s > 140 = Nothing
              | otherwise = Just ('#':s)
