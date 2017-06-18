-- Longest Common Subsequence (Performance version)
-- https://www.codewars.com/kata/593ff8b39e1cc4bae9000070

module LongestCommonSubsequence(lcs) where

import Data.Map as Map
import Control.Monad.State.Lazy as State

type StateMap a b = State (Map a b) b

memoizeM :: Ord a => ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (f x) Map.empty
    where g x = t f x >>= \y -> get >>= \m -> put (Map.insert x y m) >> return y
          f x = get >>= \m -> maybe (g x) return (Map.lookup x m)

lcsA :: Applicative a => ((String, String) -> a String) -> (String, String) -> a String
lcsA f ([], _) = pure []
lcsA f (_, []) = pure []
lcsA f (a@(x:xs), b@(y:ys)) | x == y = (x:) <$> f (xs, ys)
                            | otherwise = maxl <$> f (a, ys) <*> f (xs, b)
                            where maxl x y = if length x > length y then x else y

lcs :: String -> String -> String
lcs =  curry $ memoizeM lcsA
