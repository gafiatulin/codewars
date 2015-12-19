-- Almost Even
-- http://www.codewars.com/kata/529e2e1f16cb0fcccb000a6b/

module Split where

splitInteger :: Int -> Int -> [Int]
splitInteger a b = case diff of
    EQ -> ds
    LT -> (drop (a - (sum ds)) ds) ++ replicate (a - (sum ds)) (d+1)
    GT -> replicate ((sum ds) - a) (d-1) ++ (drop ((sum ds)- a) ds)
    where d = round (fromIntegral a / fromIntegral b)
          ds = replicate b $ d
          diff = compare (sum ds) a
