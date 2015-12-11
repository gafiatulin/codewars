-- SillyCASE
-- http://www.codewars.com/kata/552ab0a4db0236ff1a00017a/

module Codewars.Kata.SillyCASE where

import Data.Char (toUpper, toLower)

sillyCASE :: String -> String
sillyCASE = halve >>= \(a, b) -> return (map toLower a ++ map toUpper b)
        where halve xs = splitAt (fromIntegral . ceiling . (/2) . fromIntegral . length $ xs) xs
