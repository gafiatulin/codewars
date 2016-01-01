-- The observed PIN
-- http://www.codewars.com/kata/5263c6999e0f40dee200059d/

module PIN (getPINs) where

getPINs :: String -> [String]
getPINs [] = []
getPINs [x] = f x
getPINs (x:xs) = concatMap (\s -> map  (s++) . getPINs $ xs) . f $ x
f '1' = ["1", "2", "4"]
f '2' = ["1","2","3","5"]
f '3' = ["2","3","6"]
f '4' = ["1","4","5","7"]
f '5' = ["2","4","5","6","8"]
f '6' = ["3","5","6","9"]
f '7' = ["4","7","8"]
f '8' = ["5","7","8","9","0"]
f '9' = ["6","8","9"]
f  _  = ["8","0"]
