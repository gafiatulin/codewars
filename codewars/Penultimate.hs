-- Penultimate
-- http://www.codewars.com/kata/54162d1333c02486a700011d/

module Penultimate where

penultimate :: [a] -> a
penultimate = last . init
