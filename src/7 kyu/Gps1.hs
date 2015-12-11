-- Speed Control
-- http://www.codewars.com/kata/56484848ba95170a8000004d/

module Codewars.G964.Gps1 where
    
gps :: Int -> [Double] -> Int
gps _ [] = 0 
gps _ [x] = 0 
gps s xs = floor . maximum $ zipWith (\a b -> (b-a)*60*60 / fromIntegral s) xs (tail xs)