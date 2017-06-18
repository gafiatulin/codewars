-- The fusc function -- Part 2
-- https://www.codewars.com/kata/57040e445a726387a1001cf7

module Fusc(fusc) where

import Data.Map as Map
import Control.Monad.State.Lazy as State

type StateMap a b = State (Map a b) b

memoizeM :: Ord a => ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (f x) Map.empty
    where g x = t f x >>= \y -> get >>= \m -> put (Map.insert x y m) >> return y
          f x = get >>= \m -> maybe (g x) return (Map.lookup x m)

fuscM :: Applicative a => (Integer -> a Integer) -> Integer -> a Integer
fuscM f 0 = pure 0
fuscM f 1 = pure 1
fuscM f n | even n = f (n `div` 2)
          | otherwise = (+) <$> f (n `div` 2) <*> f (1 + (n `div` 2))

fusc = memoizeM fuscM
