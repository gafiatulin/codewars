-- Excel sheet column numbers
-- http://www.codewars.com/kata/55ee3ebff71e82a30000006a

module Codewars.G964.Title2Nb where

import Data.Char (ord)

titleToNb = foldl (\acc c -> acc * f 'Z' + f c) 0
    where f x = fromIntegral (ord x - ord 'A' + 1)