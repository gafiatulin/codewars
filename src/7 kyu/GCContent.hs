-- DNA GC-content
-- http://www.codewars.com/kata/5747a9bbe2fab9a0c400012f

module Codewars.Kata.GCContent where
import Codewars.Kata.GCContent.Types

import Data.List (genericLength)

-- data Base = A | C | G | T deriving (Eq, Show)

gcContent :: [Base] -> Double
gcContent [] = 0
gcContent xs = (/ (genericLength xs)) . (* 100) . genericLength . filter (`elem` [G, C]) $ xs
