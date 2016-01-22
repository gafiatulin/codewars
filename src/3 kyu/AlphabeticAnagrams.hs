-- Alphabetic Anagrams
-- http://www.codewars.com/kata/53e57dada0cb0400ba000688/

module AlphabeticAnagrams where

import Data.List (sort, group, tails, inits, nub)
import Control.Arrow ((&&&))

lexiPos :: String -> Integer
lexiPos [x] = 1
lexiPos w@(c:cs) = sum (map f ys) + lexiPos cs 
    where ys = nub . map (sort . tail) . filter ((< c) . head) . zipWith (\i t -> head t : i ++ tail t ) (init . tail . inits $ w) $ (tail . init . tails $ w)
          f = uncurry div . (g . sum &&& product . map g) . map length . group . sort
          g n = product [2.. fromIntegral n]
