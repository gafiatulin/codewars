-- Last digit of a large number
-- http://www.codewars.com/kata/5511b2f550906349a70004e1/

module LastDigit where

lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit n m = (period . (`mod`10) $ n) !! fromIntegral (m `mod` 4)
    where period :: Integer -> [Integer]
          period 2 = [6, 2, 4, 8]
          period 3 = [1, 3, 9, 7]
          period 4 = [6, 4, 6, 4]
          period 7 = [1, 7, 9, 3]
          period 8 = [6, 8, 4, 2]
          period 9 = [1, 9, 1, 9]
          period x = [x, x, x, x]
