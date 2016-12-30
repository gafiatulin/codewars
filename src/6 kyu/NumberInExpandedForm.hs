-- Write Number in Expanded Form
-- https://www.codewars.com/kata/5842df8ccbd22792a4000245/solutions/haskell

module Kata where

import Data.List (intercalate)

expandedForm :: Int -> String
expandedForm = intercalate " + " . map(\(n, c) ->  c : replicate n '0' ) . reverse . filter ((/='0') . snd) . zip [0..] . reverse . show
