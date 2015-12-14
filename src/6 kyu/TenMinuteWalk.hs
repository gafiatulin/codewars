-- Take a Ten Minute Walk
-- http://www.codewars.com/kata/54da539698b8a2ad76000228/

module Codewars.Kata.TenMinuteWalk where

import Data.Complex (magnitude, Complex( (:+) ))

isValidWalk :: String -> Bool
isValidWalk walk = case splitAt 10 walk of (ds, []) -> (length walk == 10) && 
                                                       ((==0) . magnitude . sum . map (\c -> case c of 
                                                           'n' -> 0 :+ 1
                                                           's' -> 0 :+ (-1)
                                                           'w' -> (-1) :+ 0
                                                           'e' -> 1 :+ 0) $ walk)
                                           (_, rest) -> False
