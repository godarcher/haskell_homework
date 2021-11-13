> module FoldrFusion where
>
> import Prelude hiding (map)

The fusion law for foldr states that

IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g e = foldr h (f e)

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\x xs -> f x : xs) []

----------------------------------------------
To prove:  foldr g e . map f = foldr (g . f) e

To show that foldr g e . map f = foldr (g . f) e, we can apply the fusion law using
  f ==> foldr g e
  g ==> \x xs -> f x : xs
  e ==> []
  h ==> g . f

Namely as follows:

  foldr g e . map f
              ----- rewrite map as foldr
= foldr g e . foldr (\x xs->f x : xs) []
  ---------------------------------------- foldr fusion
= foldr (g . f) (foldr (\x xs->f x : xs) e [])
                ------------------------------ definition of foldr
= foldr (g . f) e

Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that f (g x y) = h x (f y)
f (g x y) = f . g . x (y)
h x (f y) = h f . x (y)
Which is the case since:

  ...

--------------------------------------
To prove:  map (f . g) = map f . map g

h (f a b)
            ---- h = map c, f = (\x xs -> g x : xs), b = []
= map c ((\x xs -> gx : xs) a [])
= map c (g a [])
= foldr (\x xs -> f x : xs) [] (g a : [])
= 

map f . map q 
            ---- rewrite map as foldr 
= map f . foldr(\x xs -> g x : xs) []
            ---- g = map f, e = [], i = (\x xs -> g x : xs)
= g . foldr i e
            ---- g . foldr i e = foldr h (g e) (rule proven in 8.8.a)
= foldr h (g e)
            ---- rerwrite terms back to original 
= foldr h (map f [])
            ----- map f [] = []  definition of map rule
= foldr h ([])
            ----- foldr h ([])


= foldr (\x xs -> f (g x) : xs) []
            ----- definition of . rule
= foldr (\x xs -> (f . g) x : xs) []
            ----- definition of map rule
= map (f . g)
----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

