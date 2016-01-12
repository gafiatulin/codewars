-- Dictionary sequence
-- http://www.codewars.com/kata/55c24ce1fb411bbdd1000009/

module Haskell.Codewars.Solution where

import Data.Maybe (fromJust)

alphaDict :: [String]
alphaDict = map f [0..]
    where alpha = zip [0..] . map (:[]) $ ['a'..'z']++['A'..'Z']
          f x | x < 52 = fromJust . lookup x $ alpha
              | otherwise = (++r') . f . pred $ x'
              where (x', r) = x `divMod` 52
                    r' = fromJust . lookup r $ alpha
