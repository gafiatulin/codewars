-- Go so far around to the right that you end up left
-- http://www.codewars.com/kata/5424e78460d77749f2000279/

module SoFarRight where

import Prelude hiding (foldl, reverse)

foldl f z l = foldr (\b g x -> g (f x b)) id l z
