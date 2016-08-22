-- Happy^^ numbers
-- http://www.codewars.com/kata/5464cbfb1e0c08e9b3000b3e/

module IsHappy where

import Data.Char (digitToInt)

isHappy :: Integer -> Bool
isHappy = (==1) . head . filter (`elem` [1,4]) . iterate (fromIntegral . sum . map ((^2) . digitToInt) . show)
