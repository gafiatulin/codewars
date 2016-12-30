-- Master of Files
-- http://www.codewars.com/kata/574bd867d277832448000adf

module Codewars.Kata.Files (isAudio, isImage) where

import Control.Arrow ((&&&))
import Data.Char (isLetter)
import System.FilePath.Posix (takeExtension, takeBaseName)

f exts = uncurry (&&) . ( uncurry (&&) . (all isLetter &&& not . null) . takeBaseName &&& (`elem` exts) . takeExtension )

isAudio :: FilePath -> Bool
isAudio =  f [".mp3", ".flac", ".alac", ".aac"]

isImage :: FilePath -> Bool
isImage = f [".jpg", ".jpeg", ".png", ".bmp", ".gif"]
