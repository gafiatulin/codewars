-- Codewar's style ranking system
-- http://www.codewars.com/kata/51fda2d95d6efda45e00004e/

module CodewarRanking where

import Control.Monad (liftM2, join)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

data User = User { rank :: Int, progress :: Int} deriving Show

newUser :: User
newUser = User (-8) 0

incProgress :: Int -> User -> User
incProgress pr u | pr < (-8) || pr > 8 || pr == 0 = error "Error"
                 | otherwise = case join . fmap ((`lookup` ranks') . (+upr)) . lookup (rank u) $ ranks of Nothing -> User 8 0
                                                                                                          Just r  -> User r upp
                 where ranks  = zip ([(-8)..(-1)] ++ [1..8]) [0..]
                       ranks' = init . map swap $ ranks
                       (upr, upp) = (`divMod` 100) . (+ progress u) . score . fromMaybe (-2) . liftM2 (-) (lookup pr ranks) $ lookup (rank u) ranks
                       score (-1) = 1
                       score 0 = 3
                       score n | n > 0 = 10*n*n
                               | otherwise = 0 
