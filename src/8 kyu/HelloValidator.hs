-- Did she say hallo?
-- http://www.codewars.com/kata/56a4addbfd4a55694100001f/

module Codewars.Kata.HelloValidator where

import Data.List (isPrefixOf)

validateHello :: String -> Bool
validateHello cs = any (`isPrefixOf` cs) ["hello", "ciao", "salut", "hallo", "hola", "ahoj", "czesc"]
