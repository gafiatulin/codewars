-- Reverse polish notation calculator
-- http://www.codewars.com/kata/52f78966747862fc9a0009ae/

module RPN where

import Numeric (readFloat)

calc :: String -> Double
calc "" = 0
calc str = head . foldl f [] . words $ str
    where f stack v = case lookup v $ [("+", (+)), ("*", (*)), ("-", (-)), ("/", (/))] of Nothing -> (fst . head . readFloat $ v): stack
                                                                                          Just g  -> g (head . tail $ stack) (head stack) : drop 2 stack
