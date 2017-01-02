-- Calculate Fibonacci return count of digit occurrences
-- https://www.codewars.com/kata/5779f894ec8832493f00002d

module Kata (fibDigits) where

import Data.Char (digitToInt)
import Data.List (foldl', sortBy)
import qualified Data.IntMap as I
import Control.Monad.State (get, liftM2, modify, evalState, State)

fib :: Integer -> Integer
fib n | n < 0 = (if odd . succ . abs $ n then negate else id) . fib . abs $ n
      | otherwise = fibMI . fromIntegral $ n
      where fibMI :: Int -> Integer
            fibMI n = evalState (f n) I.empty
            f :: Int -> State (I.IntMap Integer) Integer
            f 0 = return 0
            f 1 = return 1
            f 2 = return 1
            f n = get >>= \m -> case I.lookup n m of
                Just v -> return v
                Nothing -> let k = n `div` 2 in if odd n then liftM2 (+) (fmap (^2) . f $ k) (fmap (^2) . f . succ $ k) else liftM2 (-) (fmap (*2) (liftM2 (*) (f k) (f . succ $ k))) (fmap (^2) . f $ k) >>= \v -> modify (I.insert n v) >> return v

f (k1, v1) (k2, v2) | k1 == k2 = compare v2 v1
                    | otherwise = compare k2 k1

fibDigits :: Integer -> [(Integer, Integer)]
fibDigits = sortBy f . map (\(k, v) -> (v, fromIntegral k)) . I.toList . foldl' (\m i -> I.insertWith (+) i 1 m) I.empty . map digitToInt . show . fib
