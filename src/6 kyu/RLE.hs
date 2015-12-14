-- Run-length encoding
-- http://www.codewars.com/kata/546dba39fa8da224e8000467/

module RLE where

import Control.Arrow ((&&&))
import Data.List (group)

runLengthEncoding :: String -> [(Int, Char)]
runLengthEncoding = map (length &&& head) . group
