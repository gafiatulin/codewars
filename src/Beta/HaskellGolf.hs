-- One Line Task: How many digits?
-- https://www.codewars.com/kata/599f84f86780ef2de7000063

module HaskellGolf where
f x=read(show(x-1)++replicate(x-1)'8'++"9")
