-- Unix command line `ls -l` extract the file type.
-- https://www.codewars.com/kata/5822b65bb81f702016000026

module Codewars.Kata.Attribute where
import Codewars.Kata.Attribute.Preload

-- data FileType = File | Directory | Symlink | Character
--               | Block | Door | Socket | Pipe
linuxType :: String -> FileType
linuxType = f . head
  where f '-' = File
        f 'd' = Directory
        f 'l' = Symlink
        f 'c' = Character
        f 'b' = Block
        f 'D' = Door
        f 's' = Socket
        f 'p' = Pipe
