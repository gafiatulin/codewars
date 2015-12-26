-- Process trees
-- http://www.codewars.com/kata/52817f04b70058a1b1000037/

module ProcessTree where

import ProcessTree.Process
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Data.List (partition)

makeTree :: [(PID, PID)] -> Process
makeTree ps = subTree rootPID . filter (/= (rootPID, -1)) $ ps
    where rootPID = fromJust . lookup (-1) . map swap $ ps
          subTree parent ps = Process parent $ (map f c)
              where (c, gc) = partition (\(pid, ppid) -> ppid == parent) ps
                    f (pid, ppid) = subTree pid gc
