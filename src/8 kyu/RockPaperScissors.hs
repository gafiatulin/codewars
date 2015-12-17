-- Rock Paper Scissors!
-- http://www.codewars.com/kata/5672a98bdbdd995fad00000f/

module Codewars.RockPaperScissors where

rps :: String -> String -> String
rps p1 p2 | p1 == p2 = "Draw!"
          | (p1, p2) `elem` [("rock", "scissors"), ("paper", "rock"), ("scissors", "paper")] = "Player 1 won!"
          | otherwise = "Player 2 won!"
