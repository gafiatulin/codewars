-- Complete The Pattern #2
-- http://www.codewars.com/kata/55733d3ef7c43f8b0700007c

module Haskell.Codewars.Pattern where

import Data.List (inits, intercalate)

pattern :: Int -> String
pattern n = intercalate "\n" 
          . map ( foldl (\acc x -> acc ++ show x) "" ) 
          . reverse . tail . inits $ [n, n-1..1]
