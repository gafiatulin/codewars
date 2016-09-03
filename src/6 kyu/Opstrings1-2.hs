-- Moves in squared strings (IV)
-- https://www.codewars.com/kata/56dbf59b0a10feb08c000227

module Codewars.G964.Opstrings1 where

import Data.List (intercalate, transpose)

rot90Counter :: String -> String
rot90Counter = intercalate "\n" . reverse . transpose . lines

diag2Sym :: String -> String
diag2Sym = intercalate "\n" . reverse . map reverse . transpose . lines

selfieDiag2Counterclock :: String -> String
selfieDiag2Counterclock str = intercalate "\n" . zipWith3 (\a b c -> a ++ "|" ++ b ++ "|" ++ c) ls (reverse . map reverse . transpose $ ls) $ (reverse . transpose $ ls)
    where ls = lines str

oper :: (String -> String) -> String -> String
oper = ($)
