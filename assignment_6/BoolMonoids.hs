module Monoids where
import Data.Monoid

-- ! EXERCISE 6.5.1

-- ? CONJUCTION MONOID
newtype Conj = Conj { getConj :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Monoids.Conj where
        Monoids.Conj f <> Monoids.Conj g = Monoids.Conj ((&&) f g)

instance Monoid Monoids.Conj where
        mempty = Monoids.Conj True
        Monoids.Conj x `mappend` Monoids.Conj y = Monoids.Conj (x && y) -- * Actual Conjunction

-- ? DISJUNCTION MONOID
newtype Disj = Disj { getDisj :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Semigroup Monoids.Disj where
        Monoids.Disj f <> Monoids.Disj g = Monoids.Disj ((||) f g)

instance Monoid Monoids.Disj where
        mempty = Monoids.Disj False
        Monoids.Disj x `mappend` Monoids.Disj y = Monoids.Disj (x || y) -- * Actual Disjunction

-- ? EXCLUSIVE OR MONOID
newtype ExclusiveOr = ExclusiveOr Bool

instance Semigroup Monoids.ExclusiveOr where 
        (Monoids.ExclusiveOr f) <> (Monoids.ExclusiveOr g) = Monoids.ExclusiveOr ((/=) f g)

instance Monoid Monoids.ExclusiveOr where 
        mempty = Monoids.ExclusiveOr False
        Monoids.ExclusiveOr x `mappend` Monoids.ExclusiveOr y = Monoids.ExclusiveOr (x /= y)

-- ! EXERCISE 6.5.2
-- ? mconcat = foldr (<>) mempty
-- * For conjunction --> mconcat = foldr (<>) Monoids.Conj True
-- * For Disjunction --> mconcat = foldr (<>) Monoids.Disj False