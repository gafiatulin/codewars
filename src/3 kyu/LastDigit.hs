-- Last digit of a huge number
-- http://www.codewars.com/kata/5518a860a73e708c0a000027/

module LastDigit (lastDigit) where

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit (x:xs) = (`mod` 10) . ((x `mod` 10)^) . f $ xs 
    where f [] = 1
          f (0:xs) = if isZero xs then 1 else 0
          f (x:xs) = case x `mod` 4 of 
            1 -> 1
            2 | isZero xs -> 1
              | isOne  xs -> 2
              | otherwise -> 4
            0 | isZero xs -> 1 
              | otherwise -> 4
            _ | isOdd  xs -> 3 
              | otherwise -> 1
          isZero [] = False
          isZero (0:xs) = not (isZero xs)
          isZero (_:xs) = False
          isOne [] = True
          isOne (1:_) = True
          isOne (_:xs) = isZero xs
          isOdd [] = True
          isOdd (x:xs) = odd x || isZero xs
