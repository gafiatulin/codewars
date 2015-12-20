-- Decimal to Factorial and Back
-- http://www.codewars.com/kata/54e320dcebe1e583250008fd/

module Codewars.Kata.Dec2Fact where
    
import Data.List (unfoldr)
import Data.Maybe(fromJust)

dec2FactString :: Integer -> String
dec2FactString n = reverse . unfoldr f $ (n, 1)
    where ns = ['0'..'9'] ++ ['A'..'Z']
          f (n, k) | n == 0 = Nothing
                   | otherwise = Just ((ns !!) . fromIntegral $ m, (d, k+1))
                   where (d, m) = n `divMod` k

factString2Dec :: String -> Integer 
factString2Dec str = sum . zipWith (\n v -> product [1..n] * fromJust (lookup v ns) ) [0..] . reverse $ str
    where ns = zip (['0'..'9'] ++ ['A'..'Z']) [0..]
