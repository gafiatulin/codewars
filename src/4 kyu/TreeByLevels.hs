-- Sort binary tree by levels
-- http://www.codewars.com/kata/52bef5e3588c56132c0003bc/

module TreeByLevels where

import TreeByLevels.TreeNode
import qualified Data.Map.Lazy as Map

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels root = Map.foldl g [] . f root $ 0
    where f Nothing _ = Map.empty
          f (Just node) l = Map.unionsWith (++) [Map.singleton l [value node], f (left node) (l+1), f (right node) (l+1)]
          g acc v = acc ++ v
