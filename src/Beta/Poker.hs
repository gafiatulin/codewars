-- Poker Hands
-- http://www.codewars.com/kata/5465e8447b8c38c6b20007b9/
-- Note: Could fail on some of the random tests due to wrong Straight detection code in the reference implementation.

module Poker where

import Data.List (sortBy, groupBy, partition)
import Data.Function (on)
import Control.Monad (liftM, liftM2)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Arrow ((***), second)

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq)
          
instance Show Suit 
    where show Spades = "S"
          show Hearts = "H"
          show Diamonds = "D"
          show Clubs = "C"

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA deriving (Eq,Ord,Enum)

instance Show Rank 
    where show r | fromEnum r < 9 = show $ fromEnum r + 2
                 | r == RJ = "J"
                 | r == RQ = "Q"
                 | r == RK = "K"
                 | r == RA = "A"

data Card = Card { rank :: Rank, suit :: Suit} deriving (Eq)

instance Show Card 
    where show (Card r s) = show r ++ show s
          showList [] = showString ""
          showList (Card r s:cs) = showString (show r) . showString (show s) . showString " " . showList cs

newtype Hand = Hand { unHand :: [Card] } deriving (Eq)

instance Show Hand 
    where show (Hand cs) = show cs

instance Read Rank
    where readsPrec _ s | "2" <= s && s<= "9" = [(toEnum $ read s - 2, "")]
                        | s == "10" = [(R10,"")]
                        | s == "J" = [(RJ,"")]
                        | s == "Q" = [(RQ,"")]
                        | s == "K" = [(RK,"")]
                        | s == "A" = [(RA,"")]

instance Read Suit
    where readsPrec _ s | s == "C" = [(Clubs,"")]
                        | s == "D" = [(Diamonds,"")]
                        | s == "H" = [(Hearts,"")]
                        | s == "S" = [(Spades,"")]

instance Read Card
    where readsPrec _ s = [(Card (read $ init s) (read [last s]),"")]

instance Read Hand
    where readsPrec _ s = [(Hand $ map read $ words s,"")]

data Category = High [Rank] | Pair Rank [Rank] | TwoPairs Rank Rank Rank | Three Rank [Rank] | Straight Rank | Flush [Rank] | FullHouse Rank Rank | Four Rank Rank | StraightFlush Rank  deriving (Eq,Ord,Show)

category h = head . catMaybes $ [straightFlush h, four h, fullHouse h, flush h, straight h, three h, twoPairs h, pair h, high h]
    where high (Hand cs) = Just . High . sortBy (flip compare) . map rank $ cs
          pair (Hand cs) = f 2 Pair id cs
          twoPairs (Hand cs) = case rs of 
              (x:y:xs) -> let (x', y') = (rank . head *** rank . head) (x, y) in Just $ TwoPairs (max x' y') (min x' y') r3
              _        -> Nothing
              where (rs, r3) = second (maximum . map rank . concat) (2 `ofAKind` cs)
          three (Hand cs) = f 3 Three id cs
          straight (Hand cs) | isStraight cs = Just . Straight . maximum . map rank $ cs
                              | otherwise = Nothing
          flush (Hand cs) | isFlush cs = Just . Flush . sortBy (flip compare) . map rank $ cs
                          | otherwise = Nothing
          fullHouse (Hand cs) = uncurry (liftM2 (FullHouse `on` (rank . head))) . (listToMaybe . fst *** listToMaybe . fst) $ (3 `ofAKind` cs, 2 `ofAKind` cs)
          four (Hand cs) = f 4 Four head cs
          straightFlush (Hand cs) | isStraight cs && isFlush cs = Just . StraightFlush . maximum . map rank $ cs
                                  | otherwise = Nothing
          isStraight cs | sorted == [R2, R3, R4, R5, RA] = True 
                        | (== length sorted) . length . groupBy (==) $ sorted = and . zipWith (\r1 r2 -> r1 == pred r2) sorted $ tail sorted
                        | otherwise = False
                        where sorted = sortBy compare . map rank $ cs
          isFlush (c:cs) = all ((== suit c) . suit) cs
          ofAKind n cs = partition ((== n) . length) . groupBy ((==) `on` rank) . sortBy (flip compare `on` rank) $ cs
          f n c g cs = liftM (`c` r2) mr1
              where (mr1, r2) = (liftM (rank . head) . listToMaybe *** g . sortBy (flip compare) . map rank . concat) (n `ofAKind` cs)

betterHand :: Hand -> Hand -> Hand
betterHand h1 h2 | category h1 >= category h2 = h1
                 | otherwise = h2
