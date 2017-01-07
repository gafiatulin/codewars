-- Holiday V - SeaSick Snorkelling
-- https://www.codewars.com/kata/57e90bcc97a0592126000064

module Kata (seaSick) where

import Data.List (group)

seaSick :: String -> String
seaSick s = if (<= length s) . (*5) . pred . length . group $ s then "No Problem" else "Throw Up"
