-- Help the bookseller !
-- http://www.codewars.com/kata/54dc6f5a224c26032800005c/

module Codewars.Kata.Bookseller where

import Codewars.Kata.Bookseller.Types

stocklist :: [Stock] -> String -> [(Char, Int)]
stocklist [] _ = []
stocklist st cs = map (\cat -> (cat, sum . map quantity . filter (\b -> (==cat) . head . code $ b) $ st)) $ cs
    where code (Stock c _) = c
          quantity (Stock _ q) = q
