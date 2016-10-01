-- Coroutines
-- https://www.codewars.com/kata/547a77a6b84a1fb8bf000211

module Coroutine where

import Control.Monad (ap, forever, when, replicateM_)
import Preloaded

-- Preloaded contains the following:
-- {-# LANGUAGE DeriveFunctor #-}
--
-- newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)
--
-- data Command r u d a =
--   Done a
-- | Out d (Coroutine r u d a)
-- | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Monad (Coroutine r u d) where
    return x = Coroutine (\k -> k (Done x))
    f >>= g  = Coroutine (\k -> apply f (\c' -> case c' of
        Done a  -> apply (g a) k
        Out d c -> k (Out d (c >>= g))
        In c -> k (In (fmap (>>= g) c))))

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine (\k -> apply p2 (\c' -> case c' of
    Done a -> k (Done a)
    Out d c -> k (Out d (p1 >>> c))
    In c -> apply (pipe2 p1 c) k))

-- It might be useful to define the following function

pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a
pipe2 p1 p2 = Coroutine (\k -> apply p1 (\c' -> case c' of
    Done a -> k (Done a)
    Out d c -> apply (c >>> p2 d) k
    In c -> k (In (fmap (`pipe2` p2) c))))

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine (\k -> k (Out v (return ())))

input :: Coroutine r v d v
input = Coroutine (\k -> k (In return))

produce :: [a] -> Coroutine r u a ()
produce =  mapM_ output

consume :: Coroutine [t] u t a -> [t]
consume x = apply x (\c' -> case c' of
    Out d c -> d : consume c
    _ -> [])

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = forever (input >>= \v -> when (p v) (output v))

limit :: Int -> Coroutine r v v ()
limit n = replicateM_ n (input >>= output)

suppress :: Int -> Coroutine r v v ()
suppress n = replicateM_ n input >> forever (input >>= output)

add :: Coroutine r Int Int ()
add = forever (input >>= \v1 -> input >>= \v2 -> output (v1 + v2))

duplicate :: Coroutine r v v ()
duplicate = forever (input >>= \v -> output v >> output v)

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = produce [ x * (x+1) `div` 2 | x <- [1..] ]
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add
