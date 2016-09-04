-- Sum of primes
-- https://www.codewars.com/kata/57679f595dfe8cb026000409

module SumOfPrimes (sumOfAnsweredQueries) where
import Control.Monad(forM_, when)
import Data.Array.ST (newArray, runSTUArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, assocs, listArray, (!))

size = 3000000

sumOfAnsweredQueries :: [(Int, Int)] -> Int
sumOfAnsweredQueries = sum . map (\(s, e) -> f ! e - f ! (s - 1))
    where f :: UArray Int Int
          f = listArray (0, length primes) (0 : primes)
          primes = scanl1 (+) $ primesToUA size

sieveUA :: Int -> UArray Int Bool
sieveUA upper = runSTUArray $ do
    let m = (upper-1) `div` 2
        r = floor . sqrt $ fromIntegral upper + 1
    sieve <- newArray (1,m) True
    forM_ [1..r `div` 2] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> writeArray sieve j False
    return sieve

primesToUA :: Int -> [Int]
primesToUA upper = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA upper]
