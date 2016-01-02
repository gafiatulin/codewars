-- Base Conversion
-- http://www.codewars.com/kata/526a569ca578d7e6e300034e/

module BaseConversion where

import Data.Maybe (fromJust)
import Data.List (unfoldr)

newtype Alphabet = Alphabet { getDigits :: String } deriving (Show)

convert :: Alphabet -> Alphabet -> String -> String
convert (Alphabet a) (Alphabet b) x | n > 0 = reverse . unfoldr f $ n
                                    | otherwise = [head b]
                                    where n = sum . zipWith (\x y -> (*y) . fromJust . lookup x $ a' ) (reverse x) . iterate (*baseA) $ 1
                                          baseA = fromIntegral . length $ a
                                          baseB = fromIntegral . length $ b
                                          a' = zip a [0..]
                                          b' = zip [0..] b
                                          f 0 = Nothing
                                          f n = Just . (\(x, y) -> (fromJust . lookup y $ b', x)) . divMod n $ baseB
