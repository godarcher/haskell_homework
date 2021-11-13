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
-- ! Exercise 3.7.2
Yes it is actually more efficient.
When comparing inorder(skewed x) with
inorder'(skewed x) with an high enough x
we witness that the second function is faster
and more efficient
-}
-- TODO add equatorial reasoning

-- ? The new main ordering function
inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

-- ! Exercise 3.7.3
elemsCat :: Tree a-> [a] -> [a]
elemsCat Leaf xs = xs
elemsCat (Node x l r) xs = xs ++ elemsCat r (elemsCat l [x])

elems' :: Tree a -> [a]
elems' inp = elemsCat inp []