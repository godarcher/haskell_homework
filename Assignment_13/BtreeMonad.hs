{-# LANGUAGE InstanceSigs #-} 
module BtreeMonad where 

data Btree a = Tip a | Bin (Btree a) (Btree a)
  deriving Show 

instance Functor Btree where
  fmap :: (a -> b) -> Btree a -> Btree b
  --fmap f (Tip x) = Tip (f x)
  --fmap f (Bin l r) = Bin (fmap f l) (fmap f r) 
  fmap f tree = pure f <*> tree 

instance Applicative Btree where
  pure :: a -> Btree a 
  --pure x = Tip x 
  pure x = return x
  
  (<*>) :: Btree (a -> b) -> Btree a -> Btree b
  --(Tip f) <*> tree = fmap f tree 
  --(Bin fl fr) <*> tree = Bin (fl <*> tree) (fr <*> tree) 
  mf <*> mx = do { f <- mf; x <- mx; return (f x) }

instance Monad Btree where
  return :: a -> Btree a 
  return x = Tip x 

  (>>=) :: Btree a -> (a -> Btree b) -> Btree b
  Tip x >>= f   = f x
  Bin l r >>= f = Bin (l >>= f) (r >>= f) 
