-- Password maker
-- http://www.codewars.com/kata/5637b03c6be7e01d99000046

module PasswordMaker where

makePassword :: String -> String
makePassword  =  map (f . head) . words
    where f c | c `elem` "Ii" = '1'
              | c `elem` "Oo" = '0'
              | c `elem` "Ss" = '5'
              | otherwise = c
