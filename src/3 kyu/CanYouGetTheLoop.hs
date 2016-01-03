-- Can you get the loop ?
-- http://www.codewars.com/kata/52a89c2ea8ddc5547a000863/

module CanYouGetTheLoop where

import CanYouGetTheLoop.Types
import Data.List (elemIndices)
import Control.Arrow ((***))

loopSize :: Eq a => Node a -> Int
loopSize a = (\(x:y:_) -> y-x) . elemIndices (fst . head . dropWhile (uncurry (/=)) . tail . iterate (next *** next . next) $ (a, a)) . iterate next $ a
