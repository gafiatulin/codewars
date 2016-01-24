-- Huffman Encoding
-- http://www.codewars.com/kata/54cf7f926b85dcc4e2000d9d/

module Huffman (frequencies, encode, decode, Bit (..)) where

import Data.List (group, insertBy, sortBy)
import qualified Data.Map.Strict as M
import Control.Monad (liftM, join)
import Data.Function (on)
import Control.Arrow ((&&&), second)

data Bit = Z | O deriving (Eq, Show)
data HTree a = Leaf a Int | Node (HTree a) (HTree a) Int deriving (Show)
type Codemap a = M.Map a [Bit]

weight :: HTree a -> Int
weight (Leaf _ x) = x
weight (Node _ _ x) = x

merge :: HTree a -> HTree a -> HTree a
merge t1 t2 = Node t1 t2 (weight t1 + weight t2)

buildTree :: Ord a => [(a, Int)] -> Maybe (HTree a)
buildTree = f . map (uncurry Leaf)
    where f [] = Nothing
          f [Leaf _ _] = Nothing
          f [x] = Just x
          f (x:y:xs) = f . insertBy (flip compare `on` weight) (merge x y) $ xs

codemap :: Ord a => HTree a -> Codemap a
codemap (Leaf v _) = M.singleton v []
codemap (Node l r w) = M.map (Z:) (codemap l) `M.union` M.map (O:) (codemap r)

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = sortBy (compare `on` snd) . map (head &&& length) . group . sortBy compare

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode [] _ = Nothing
encode [_] _ = Nothing
encode fs as = liftM concat . mapM (\c -> join . liftM (M.lookup c) $ cm) $ as
    where cm = fmap codemap (buildTree fs)

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a => [(a, Int)] -> [Bit] -> Maybe [a]
decode fs bs = join . liftM (`f` bs) $ tree
    where tree = buildTree fs
          f _ [] = Just []
          f tree bs = join . liftM ((\(a, rest) -> liftM (a:) rest )  . second (f tree)) . g tree $ bs
          g (Leaf v _) xs = Just (v, xs)
          g (Node {}) [] = Nothing
          g (Node l _ _) (Z : xs) = g l xs
          g (Node _ r _) (O : xs) = g r xs
