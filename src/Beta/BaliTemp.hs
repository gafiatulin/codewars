-- Holiday I - Temperature in Bali
-- http://www.codewars.com/kata/57e8e9df2aee49c0280009ab

module BaliTemp where

bareable :: Int -> Float -> Bool
bareable heat humidity | (heat >35) || (humidity > 0.5) || (humidity >0.4) && (heat >25) && (heat <36) = False
                       | otherwise = True
