module Monoids where
import Data.Monoid

-- ! CONJUCTION MONOID
newtype Conj = Conj { getConj :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Monoids.Conj where
    Monoids.Conj f <> Monoids.Conj g = Monoids.Conj ((&&) f g)

instance Monoid Monoids.Conj where
        mempty = Monoids.Conj True
        Monoids.Conj x `mappend` Monoids.Conj y = Monoids.Conj (x && y) -- * Actual Conjunction

-- ! DISJUNCTION MONOID
newtype Disj = Disj { getDisj :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Monoids.Disj where
    Monoids.Disj f <> Monoids.Disj g = Monoids.Disj ((||) f g)

instance Monoid Monoids.Disj where
        mempty = Monoids.Disj False
        Monoids.Disj x `mappend` Monoids.Disj y = Monoids.Disj (x || y) -- * Actual Disjunction
--newtype ... = ...

--instance Semigroup ... where

--instance Monoid ... where
