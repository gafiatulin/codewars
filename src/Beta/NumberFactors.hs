-- Number factors
-- https://www.codewars.com/kata/57a19defbb994481c40001bd

module NumberFactors where

getFactors :: Int -> [Int]
getFactors n = divisors n ++ (concatMap (\ x -> if x ^ 2 == n then [] else [n `div` x]) . reverse $ divisors n)
    where divisors n = 1 : filter ((==0) . rem n) [2 .. floor . sqrt . fromIntegral $ n]
