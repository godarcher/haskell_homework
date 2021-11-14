> module Btree where
import qualified GHC.Num.BigNat as Q.E.D
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: Btree a -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

To prove: map f (tips t) = tips (mapBtree f t) for all f,t

Base case (Tip x)
= tips (mapBtree f Tip(x))
    
= tips (Tip(f x))
        ---- f = tips(Tip(x))
= f x 
        ---- definiton of map
= map f x 
        ----tips(Tip(x)) = f as stated above
= map f (tips (Tip x))
Q.E.D. 

Inductive case (Bin as bs)