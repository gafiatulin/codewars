-- Format Text
-- https://www.codewars.com/kata/559c7b6e3c38b1d1b900006f

module Codewars.Kata.Format where

import Data.List (unfoldr, intercalate)

format :: String -> Int -> String
format str n = intercalate "\n" . unfoldr f $ ("", words str)
    where h "" x = x
          h str x = str ++ " " ++ x
          f ("", []) = Nothing
          f (acc, []) = Just (acc, ("", []))
          f (acc, w:ws) | (<= n) . length . h acc $ w = f (h acc w, ws)
                        | otherwise = Just (acc, ("", w:ws))
