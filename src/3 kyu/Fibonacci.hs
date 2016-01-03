-- The Millionth Fibonacci Kata
-- http://www.codewars.com/kata/53d40c1e2f13e331fc000c26/

module Fibonacci where

import qualified Data.IntMap as I
import Control.Monad.State (get, liftM, liftM2, modify, evalState, State)

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
                Nothing -> let k = n `div` 2 in if odd n then liftM2 (+) (liftM (^2) . f $ k) (liftM (^2) . f . succ $ k) else liftM2 (-) (liftM (*2) (liftM2 (*) (f k) (f . succ $ k))) (liftM (^2) . f $ k) >>= \v -> modify (I.insert n v) >> return v
