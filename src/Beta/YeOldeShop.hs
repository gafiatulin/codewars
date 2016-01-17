-- Shop Inventory Manager
-- http://www.codewars.com/kata/55d1d06def244b18c100007c/

module Codewars.Kata.YeOldeShop where
import Codewars.Kata.YeOldeShop.Item (Item(..)) 

import Data.List (isInfixOf)
import Data.Char (toLower)

update :: [Item] -> [Item]
update = map u
    where u item @ (Item name sellIn quality) | ("sulfuras" `isInfixOf`) . map toLower $ name = item
                                              | ("aged brie" `isInfixOf`) . map toLower $ name = Item name (pred sellIn) (if quality > 50 then quality else min 50 (succ quality)) 
                                              | ("backstage passes" `isInfixOf`) . map toLower $ name = Item name (pred sellIn) (if sellIn <= 0 then 0 else if quality > 50 then quality else min 50 (f sellIn quality))
                                              | ("conjured" `isInfixOf`) . map toLower $ name = Item name (pred sellIn) (max 0 (g 2 sellIn quality))
                                              | otherwise = Item name (pred sellIn) (max 0 (g 1 sellIn quality))
          g m d | d <= 0    = (+ m*(-2))
                | otherwise = (+ m*(-1))
          f d | d <= 5    = (+3)
              | d <= 10   = (+2)
              | otherwise = succ
