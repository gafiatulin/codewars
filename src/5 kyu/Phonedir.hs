-- Phone Directory
-- https://www.codewars.com/kata/56baeae7022c16dd7400086e

module Codewars.G964.Phonedir(phone) where

import Data.Char(isDigit)
import Data.List(intercalate)
import Data.Maybe(listToMaybe, mapMaybe)
import Control.Applicative((<|>))
import Text.ParserCombinators.ReadP(readP_to_S, satisfy, get, many, char, many1, pfail, sepBy1, between)

data Person = Person { name :: String, pNumber :: String, address :: String}
instance Show Person where
    show (Person n p a) = "Phone => " ++ p ++ ", " ++
                          "Name => " ++ n ++  ", " ++
                          "Address => " ++ a

addressChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ".-"

process = fmap record . listToMaybe . map fst . filter (null . snd) . readP_to_S phoneNameRest
    where phoneNameRest = many get >>= \a -> nameOrPhone >>= \b -> many get >>= \c -> nameOrPhone >>= \d -> many get >>= \e -> return (if isDigit . head $ b then (b, d) else (d, b), unwords [a, c, e])
          phoneNumber = char '+' >> sepBy1 (many1 digit) (char '-') >>= \ds -> if validPhone ds then return . intercalate "-" $ ds else pfail
          validPhone x = length x == 4 && ((\(c, ns) -> all (<=2) c && ns == [3,3,4]) . splitAt 1 . map length $ x)
          nameOrPhone = between (char '<') (char '>') (many1 get) <|> phoneNumber
          digit = satisfy isDigit
          record ((p, n), rest) = Person {name = n, pNumber = p, address = clean rest}
          clean = unwords . filter (not . null) . map (filter (`elem` addressChars )) . words . map (\c -> if c == '_' then ' ' else c)

phone :: String -> String -> String
phone dr num = present . filter ((==num) . pNumber) . mapMaybe process . lines $ dr
    where present [] = "Error => Not found: " ++ num
          present [x] = show x
          present (x:xs) = "Error => Too many people: " ++ num
