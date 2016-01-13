-- When?
-- http://www.codewars.com/kata/569218bc919ccba77000000b/

module Codewars.G964.Dateamount where

import Data.Time (fromGregorian, addDays, showGregorian)

dateNbDays :: Double -> Double -> Double -> String
dateNbDays a0 a = showGregorian . (`addDays` fromGregorian 2016 1 1) . ceiling . (log (a/a0) /) . log . succ . (/36000)
