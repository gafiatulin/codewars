-- Buying a car
-- http://www.codewars.com/kata/554a44516729e4d80b000012/

module Codewars.Kata.BuyCar where

nbMonths :: Integer -> Integer -> Integer -> Double -> [Integer]
nbMonths spo spn s l = (\(o, n, m, _) -> [fromIntegral m, fromIntegral . round $ (fromIntegral m * s' + o - n)] ) 
                     . head . dropWhile (\(o, n, m, _) -> fromIntegral m * s' + o <= n) 
                     . iterate next $ (spo', spn', 0, l)
    where spo' = fromIntegral spo
          spn' = fromIntegral spn
          s' = fromIntegral s
          next (spo', spn', m, l) = (perc l * spo', perc l * spn', m + 1 , if even m then l + 0.5 else l)
          perc l = 1 - 0.01 * l
