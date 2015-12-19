-- Wind component calculation
-- http://www.codewars.com/kata/542c1a6b25808b0e2600017c/

module WindInfo where

import Numeric (readDec)
import Control.Arrow ((&&&))

windInfo :: String -> Int -> Int -> String
windInfo r d s = toStr . (round . (*s') . cos &&& round . (*s') . sin) . (*(pi/180)) . fromIntegral . (d -) . (*10) . fst . head . readDec $ r
    where s' = fromIntegral s
          toStr (a, b) = (if a >= 0 then "Headwind " else "Tailwind ") 
                       ++ (show . abs $ a) ++ " knots. Crosswind " 
                       ++ (show . abs $ b) ++ " knots from your " 
                       ++ (if b >= 0 then "right" else "left") ++ "."
