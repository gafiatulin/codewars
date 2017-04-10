-- Move-To-Front Encoding
-- http://www.codewars.com/kata/54ce6115975ca685dd0005d5/

module MTF where

import Data.List (unfoldr, elemIndex)
import Control.Monad (join, liftM, liftM2)
import Data.Maybe (listToMaybe)

encode :: Eq a => [a] -> [a] -> Maybe [Int]
encode alph xs = sequence . unfoldr f $ (xs, Just alph)
    where f ([], _) = Nothing
          f (c : cs, ma) = let mi = join $ fmap (elemIndex c) ma in Just (mi, (cs, liftM2 (\i a -> a!!i : take i a ++ drop (i+1) a ) mi ma))

decode :: Eq a => [a] -> [Int] -> Maybe [a]
decode alph is | all (>=0) is = sequence . unfoldr f $ (is, Just alph)
               | otherwise = Nothing
               where f ([], _) = Nothing
                     f (i: is, ma) = let me = join . liftM (\a -> listToMaybe . drop i $ a) $ ma in Just (me, (is, liftM2 (\e a -> e : take i a ++ drop (i+1) a ) me ma ))
