-- Moves in squared strings (III)
-- https://www.codewars.com/kata/56dbeec613c2f63be4000be6

module Codewars.G964.Opstrings1 where

import Data.List (intercalate, transpose)

rot90Clock :: String -> String
rot90Clock = intercalate "\n" . map reverse . transpose . lines

diag1Sym :: String -> String
diag1Sym = intercalate "\n" . transpose . lines

selfieAndDiag1 :: String -> String
selfieAndDiag1 str = intercalate "\n" . zipWith (\a b -> a ++ "|" ++ b) ls . transpose $ ls
    where ls = lines str

oper :: (String -> String) -> String -> String
oper = ($)
