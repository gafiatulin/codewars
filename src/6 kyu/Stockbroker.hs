-- Ease the StockBroker
-- http://www.codewars.com/kata/54de3257f565801d96001200/

module Codewars.G964.Stockbroker where

import Data.Char (isDigit, isSpace)
import Data.Either (partitionEithers)
import Data.List (elemIndices, partition, groupBy)
import Data.List.Split (split, dropDelims, dropBlanks, oneOf)

balanceStatements "" = "Buy: 0 Sell: 0"
balanceStatements s = f correct ++ g malformed
    where (malformed, correct) = partitionEithers . map validate . split (dropDelims $ oneOf ",") $ s
          f = concat . (\(b, s) -> [("Buy: "++) . show . sum . map snd $ b , (" Sell: "++) . show . sum . map snd $ s]) . partition ((=="B") . fst)
          g xs = if null xs then "" else concat . (\x -> [("; Badly formed "++) . (++": ") . show . length $ x, concat x]) . map ((++" ;") . dropWhile isSpace) $ xs
          validAction "B" = True
          validAction "S" = True
          validAction _ = False
          validInt :: String -> Bool
          validInt = all isDigit
          validDouble :: String -> Bool
          validDouble = (\(ds, nds) -> ((>=2) . length $ ds) && ((==1) . length $ nds) && ((=='.') . head $ nds)) . partition isDigit
          validate :: String -> Either String (String, Integer)
          validate order | validOrder = Right (action, orderValue quantity price)
                         | otherwise = Left order
                         where validOrder = validInt quantity && validDouble price && validAction action
                               orderValue q p = round ((read q :: Double) * (read p :: Double))
                               (stock, quantity, price, action) = case split (dropDelims . dropBlanks $ oneOf " " ) order of
                                   (a:b:c:d:[]) -> (a, b, c, d)
                                   x -> ("", "", "", "E")
