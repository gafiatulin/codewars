-- Explosive Sum
-- http://www.codewars.com/kata/52ec24228a515e620b0005ef/

module ExplosiveSum where

explosiveSum :: Integer -> Integer
explosiveSum n | n < 0 = 0
               | otherwise = f . fromIntegral $ n
f = (map g [0 ..] !!)
    where g 0 = 1
          g 1 = 1
          g n = sum . zipWith (\f x -> f x) (cycle [id, id, negate, negate]) . map explosiveSum . takeWhile (>=0) . map (\k -> (n-) . (`div` 2) $ k*(3*k-1)) . concatMap (\x -> [x, -x]) $ [1..]
