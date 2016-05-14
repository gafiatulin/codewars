-- Credit card issuer checking
-- http://www.codewars.com/kata/5701e43f86306a615c001868

module Haskell.Codewars.CreditCardIssuer where
import Data.List (isPrefixOf)

getIssuer :: Int -> String
getIssuer n | length s == 16 && (any (`isPrefixOf` s) . map show $ [51..55]) = "Mastercard"
            | length s == 15 && any (`isPrefixOf` s) ["34", "37"] = "AMEX"
            | elem (length s) [13, 16] && isPrefixOf "4" s = "VISA"
            | length s == 16 && isPrefixOf "6011" s = "Discover"
            | otherwise = "Unknown"
            where s = show n