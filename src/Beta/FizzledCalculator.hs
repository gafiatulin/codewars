-- The Student and the Fizzled Calculator (Check whether a product is smaller than a certain number)
-- http://www.codewars.com/kata/551d9695bf4e5283c70005b9/

module Codewars.Kata.FizzledCalculator where

import Prelude hiding ((*), (/), product)

fizzledCalculator :: Double -> Double  -> Double -> Bool
fizzledCalculator x y z = (<z) . (if signum x /= signum y then negate else id) $ exp (log (abs x) + log (abs y))
