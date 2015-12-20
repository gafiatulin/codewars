-- Conway's Look and Say - Generalized
-- http://www.codewars.com/kata/530045e3c7c0f4d3420001af/

module LookAndSay where

import Data.List (group)
import Control.Arrow ((&&&))

lookSay :: Integer -> Integer
lookSay = read . concatMap (uncurry (++) . ((show . length) &&& ((: []) . head))) . group . show
