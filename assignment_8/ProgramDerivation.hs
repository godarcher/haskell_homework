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
inorderCat [] xs = xs
inorderCat (Node l a r) xs = inorderCat l [a] ++ inorderCat r xs

-- ? The new main ordering function
inorderFast :: Tree a -> [a]
inorderFast inp = inorderCat inp []
{-
inorderCat EMPTY xs = [] ++ xs

inorderCat (Node l a r) xs = inorder l ++ [a] ++ inorder r ++ xs =
induction hypothesis tells us the equivalence holds for tree l and r
inorderCat l [a] ++ inorderCat r xs

-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat t xs = inorder t ++ xs -- TODO: make me more efficient

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

