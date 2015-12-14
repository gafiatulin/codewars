-- Calculate the function f(x) for a simple linear sequence (Easy)
-- http://www.codewars.com/kata/5476f4ca03810c0fc0000098/

module Codewars.Kata.LinearSequence where

getFunction :: [Integer] -> Maybe String
getFunction values = if all (== head derivatives) (tail derivatives)
                     then Just ("f(x) = " ++ f (head derivatives) (head values))
                     else Nothing
    where derivatives = zipWith (-) (tail values) values
          f x n = case x of 0 -> if n == 0 then "" else show n
--                          0 -> show n
                            1 -> g n
                            a -> show a ++ g n
          g n | n == 0 = "x"
--            | n > 0 = "x + " ++ show n
--            | otherwise = "x - " ++ (show . abs $ n)
              | otherwise = "x + " ++ show n
