-- Base64 Numeric Translator
-- http://www.codewars.com/kata/5632e12703e2037fa7000061/

module Codewars.Kata.Base64 where

import Data.Maybe (fromJust)

base64ToBase10 :: String -> Integer
base64ToBase10 = sum . zipWith (*) [64^i | i <- [0..]] . reverse . map (fromJust . flip lookup alphabet)
    where alphabet = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" [0..]
