-- Binary Tree Traversal
-- http://www.codewars.com/kata/5268956c10342831a8000135/solutions/haskell

module BinaryTreeTraversal
  ( preOrder
  , inOrder
  , postOrder
  ) where

import BinaryTreeTraversal.Types

preOrder :: Tree a -> [a]
preOrder (Nil) = []
preOrder (Node l v r) = (v : preOrder l) ++ preOrder r

inOrder :: Tree a -> [a]
inOrder (Nil) = []
inOrder (Node l v r) = inOrder l ++ (v : inOrder r)

postOrder :: Tree a -> [a]
postOrder (Nil) = []
postOrder (Node l v r) = postOrder l ++ postOrder r ++ [v]
