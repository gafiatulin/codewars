-- Share prices
-- http://www.codewars.com/kata/5603a4dd3d96ef798f000068/

module Codewars.G964.SharesPrice where

import Text.Printf (printf)

sharePrice :: Double -> [Double] -> String
sharePrice invested [] = printf "%.2f" ((*0.01) . fromIntegral . round . (100*) $ invested :: Double)
sharePrice invested (x:xs) = sharePrice (invested + 0.01 * invested * x) xs
