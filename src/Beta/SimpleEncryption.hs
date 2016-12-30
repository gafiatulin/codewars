-- Encrypt this!
-- https://www.codewars.com/kata/5848565e273af816fb000449

module SimpleEncryption where

import Data.Char (ord)

encryptThis :: String -> String
encryptThis = unwords . map f . words
  where f [] = []
        f (x: xs) = (show . ord $ x) ++ (g xs)
        g [] = []
        g [x] = [x]
        g (x:xs) = (last xs) : (init xs) ++ [x]
