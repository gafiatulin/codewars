-- Exclamation marks series #17: Put the exclamation marks and question marks to the balance, Are they balanced?
-- https://www.codewars.com/kata/57fb44a12b53146fe1000136

module Kata (balance) where

import Prelude hiding (Either(..))
import Preloaded(Comparison(..))

balance :: String -> String -> Comparison
balance l r | f l < f r = Right
            | f l > f r = Left
            | otherwise = Balance

f = sum . map g
g '?' = 3
g '!' = 2
g _ = 0
