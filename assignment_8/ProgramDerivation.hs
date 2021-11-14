module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
for inorderCat Leaf, we doe the same as inorder but then with xs
[] + xs (definition of inorder)

inorderCat (Node x l r) xs = inorderCat l [x] ++ inorderCat r xs
---- IH states that the equivalence holds for both the left (l)
and the right tree

as inorder inp ++ [] = inorder inp
-}
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
inorder' inp = inorderCat inp []

elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

{-
elemsCat inp xs = xs ++ elems t 
      ----- the definition of elems
= xs ++ [x] ++ elems l ++ elems r 
      ----- (++) associativity
= xs ++ ([x] + elems l) ++ elems r 
      ----- IH
= xs ++ (elemsCat l [x]) ++ elems r
      ----- (++) associativity
= xs ++ ((elemsCat l [x]) ++ elems r)
      ----- IH
= xs ++ elemsCat r  (elemsCat l [x])

-}
-- ! Exercise 3.7.3
elemsCat :: Tree a-> [a] -> [a]
elemsCat Leaf xs = xs
elemsCat (Node x l r) xs = xs ++ elemsCat r (elemsCat l [x])

elems' :: Tree a -> [a]
elems' inp = elemsCat inp []