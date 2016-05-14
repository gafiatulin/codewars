-- Unlucky Days
-- http://www.codewars.com/kata/56eb0be52caf798c630013c0

module Haskell.Codewars.UnluckyDays where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

unluckyDays :: Integer -> Int
unluckyDays year = length . filter (\(_,_,x) -> x == 5) . map (toWeekDate . (\x -> fromGregorian year x 13)) $ [1..12]