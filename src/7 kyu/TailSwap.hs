-- Tail Swap
-- https://www.codewars.com/kata/5868812b15f0057e05000001

module Kata(tailSwap) where

import Data.List (break)

tailSwap :: (String, String) -> (String, String)
tailSwap (a, b) = (aa ++ bb, ba ++ ab)
    where (aa, ab) = break (== ':') a
          (ba, bb) = break (== ':') b
