-- Compare Versions
-- http://www.codewars.com/kata/53b138b3b987275b46000115/

module Codewars.Exercise.Compare where

import Data.List (unfoldr)
import Data.Char (isDigit)
import Control.Arrow ((***))

compareVersions :: String -> String -> Ordering
compareVersions v1 v2 = compare (f v1) (f v2)
    where f :: String -> [Integer]
          f = unfoldr (\vStr -> if null vStr 
                                then Nothing 
                                else Just . (read *** dropWhile (not . isDigit)) . span isDigit $ vStr)
