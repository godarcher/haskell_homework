module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

-- ! The new upgraded helper function
inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf xs = xs
inorderCat (Node x l r) xs = inorderCat l [x] ++ inorderCat r xs

{-
TODO: do tests here
-}

-- ? The new main ordering function
inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

