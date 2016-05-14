-- Wordify an integer
-- http://www.codewars.com/kata/553a2461098c64ae53000041

module Codewars.Kirilloid.WordifyInteger(wordify) where

import Control.Arrow((***))

until20 = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

wordify :: Int -> String
wordify n | n <= 19 = until20 !! n
          | n < 100 = f . ((tens !!) *** wordify) . (`divMod` 10) $ n
          | otherwise = f . ((++ " hundred") . (until20 !!) *** wordify) . (`divMod` 100) $ n
          where f = unwords . filter (not . null) . uncurry (\a b -> [a, b])