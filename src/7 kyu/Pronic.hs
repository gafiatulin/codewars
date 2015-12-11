-- Figurate Numbers #2 - Pronic Number
-- http://www.codewars.com/kata/55b1e5c4cbe09e46b3000034/

module Codewars.Exercise.Pronic where

import Control.Arrow ((***))

isPronic :: Integer -> Bool
isPronic n | n == 0  = True
           | odd n   = False
           |otherwise = uncurry (\k b -> (k == uncurry (*) b) &&  (1 == uncurry (-) b) ) 
                      . uncurry (\a b ->  (a, ( ceiling Control.Arrow.*** floor)
                                              . (\x -> (x, x)) 
                                              . sqrt 
                                              . fromIntegral $ b)) 
                      . (\x -> (x, x)) $ n
