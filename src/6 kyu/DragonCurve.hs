-- Dragon's Curve
-- http://www.codewars.com/kata/53ad7224454985e4e8000eaa/

module DragonCurve where

dragon :: Int -> String
dragon n | n < 0 = ""
         | otherwise = filter (not . flip elem "ab") . f "Fa" $ n
         where f [] _ = []
               f xs 0 = xs
               f xs n = f (concatMap g xs) (n-1)
               g c | c == 'a' = "aRbFR"
                   | c == 'b' = "LFaLb"
                   | otherwise = [c]
