-- foldMap all the things!
-- http://www.codewars.com/kata/543d218022e0f307fb000173/

module Foldmap where

import Data.Foldable (foldMap, Foldable)
import Data.Monoid

myToList :: Foldable t => t a -> [a]
myToList = foldMap return

newtype MyMin a = MyMin {getMyMin :: Maybe a}

instance Ord a => Monoid (MyMin a) where
    mempty = MyMin Nothing
    MyMin (Just x) `mappend` MyMin (Nothing) = MyMin (Just x)
    MyMin (Just x) `mappend` MyMin (Just y) = MyMin . Just . min x $ y
    MyMin (Nothing) `mappend` x = x

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = getMyMin . foldMap (MyMin . Just)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f b = flip appEndo b . foldMap (Endo . f)
