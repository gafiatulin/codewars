-- Parse float
-- https://www.codewars.com/kata/57a386117cb1f31890000039

module ParseFloat where

import Numeric (readFloat)
import Data.Maybe (listToMaybe)

parseFloat :: String -> Maybe Float
parseFloat = fmap fst . listToMaybe . readFloat
