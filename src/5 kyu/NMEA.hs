-- NMEA checksum
-- http://www.codewars.com/kata/54249c6bf132dcc661000495/

module NMEA where

import Data.Char (isHexDigit)
import Numeric (readHex)
import Data.Bits (xor)

check :: String -> Bool
check ('$':xs) | valid = (== (fst . head . readHex $ checksum)) . foldl1 xor . map fromEnum $ str
    where (str, _:rest) = break (=='*') xs 
          (checksum, end) = span isHexDigit rest
          valid = (end == "\r\n") && (length checksum == 2)
check x = False
