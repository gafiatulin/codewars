-- Pyramid Slide Down
-- http://www.codewars.com/kata/551f23362ff852e2ab000037/

module PyramidSlideDown where

longestSlideDown :: [[Int]] -> Int
longestSlideDown = head . foldr1 (\v acc  -> zipWith (+) v (zipWith max acc (tail acc)))
