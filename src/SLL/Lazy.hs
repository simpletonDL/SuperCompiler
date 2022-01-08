module SLL.Lazy where

data Tree =
    Leaf
  | Node Int Tree Tree
 deriving (Eq, Show)

tree :: Int -> Tree
tree x = Node x (tree $ x + 1) (tree $ x + 1)
