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
Note: We take a as name for x here because otherwise a is hard to distinguish for x in \x xs:
Second Note: after writing everything out I found out this spelled gay, I just choose 'a' because it is the
first letter in the alphabet, no joke was intended with this.
so f(g a y) = h a (f y)
Which is the case since:

f (g a y) 
            ---- f = map f, g = (\x xs -> g x : xs), y = []
= map f ((\x xs -> g x :xs) a [])
            ---- simpl
= map f (g a : [])
            ---- definition of foldr
= foldr (\x xs -> f x : xs) [] (g a : [])
= (\x xs -> f x : xs) (g a) (foldr (\x xs -> f x : xs) [] [])
            ---- definition of map (rewrite foldr to map)
= (\x xs -> f x : xs) (g a) (map f [])
            ---- function
= f (g a) : map f []
            ---- definition of :
= (\x xs -> f (g x) : xs) a (map f [])

= h a (f y)


h = \x xs -> f (g x) : xs ---- we use this in our second proof


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
            ----- rule proofed above that h = \x xs -> f (g x) : xs
= foldr (\x xs -> f (g x) : xs) []
            ----- definition of . rule
= foldr (\x xs -> (f . g) x : xs) []
            ----- definition of map rule
= map (f . g)
----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

