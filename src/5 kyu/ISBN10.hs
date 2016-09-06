-- ISBN-10 Validation
-- https://www.codewars.com/kata/51fc12de24a9d8cb0e000001

module ISBN10 where

import Data.Char (digitToInt)

validISBN10 :: String -> Bool
validISBN10 str = length str == 10 && f str && g str
  where f x = (all (`elem` ['0'..'9']) . init $ x) && ((`elem` 'X':['0'..'9']) . last $ x)
        g = (== 0) . (`mod` 11) . sum . zipWith (*) [1 ..] . map h
        h 'X' = 10
        h c = digitToInt c
