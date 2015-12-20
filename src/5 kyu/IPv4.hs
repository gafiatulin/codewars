-- int32 to IPv4
-- http://www.codewars.com/kata/52e88b39ffb6ac53a400022e/

-- Note: Wrong type in tests, should be Word32 instead of Int32.

module IPv4 where

import Data.Word (Word32)
import Data.Bits ((.&.), shiftR)
import Data.List (unfoldr, intercalate)

type IPString = String

int32ToIP :: Word32 -> IPString
int32ToIP n = intercalate "." . map show . reverse . take 4 . (++ repeat 0) . unfoldr f $ n
    where f 0 = Nothing
          f x = Just ((.&.) x 0xff, shiftR x 0x8)