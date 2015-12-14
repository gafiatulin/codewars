-- Sqrt approximation
-- http://www.codewars.com/kata/52ecde1244751a799b00018a/

module CodeWars.Sqrt where
import Prelude hiding (sqrt)

sqrtInt :: Integral n => n -> Either (n,n) n
sqrtInt n = case compare n (k^2) of EQ -> Right k
                                    LT -> Left (k-1, k)
                                    GT -> Left (k, k+1)
    where n' = fromIntegral n
          k = round $ sqrt' n' n'
          sqrt' n guess | abs (guess^2 - n) < 1 = guess
                        | otherwise = sqrt' n (next guess)
                        where next g = (g + n / g) / 2
