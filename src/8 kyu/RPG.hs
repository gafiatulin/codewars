-- Online RPG: player to qualifying stage?
-- http://www.codewars.com/kata/55849d76acd73f6cc4000087

module Codewars.Kata.RPG where

playerRankUp :: Integer -> String
playerRankUp points = if points >= 100 then "Well done! You have advanced to the qualifying stage. Win 2 out of your next 3 games to rank up." else "False"
