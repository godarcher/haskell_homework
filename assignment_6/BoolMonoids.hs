module Monoids where
import Data.Monoid

-- | Boolean monoid under conjunction.
newtype All = All { getAll :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Monoids.All where
        mempty = Monoids.All True
        Monoids.All x `mappend` Monoids.All y = Monoids.All (x && y)

-- | Boolean monoid under disjunction.
newtype Any = Any { getAny :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
        mempty = Monoids.Any False
        Monoids.Any x `mappend` Monoids.Any y = Monoids.Any (x || y)
--newtype ... = ...

--instance Semigroup ... where

--instance Monoid ... where
