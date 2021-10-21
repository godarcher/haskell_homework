module DigitalSorting where

import Data.List
import Data.Bool
import Data.Maybe
import Data.Either

-- ! our extra input
import Data.Map
import qualified Data.Bifunctor
class Rankable key where
  rank :: [(key,a)] -> [[a]]

-- ! Standard functions that are given
digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . Data.List.map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

-- ! Exercise 6.6.1
genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank key = [b|(a, b) <- toList (fromListWith (++) [(a, [b]) | (a, b) <- key])]

-- ! Exercise 6.6.7
-- * This is higher because functions need to be higher 
-- ? We use a lambda expression to add an extra key then do rank 
-- ? This method will lead to removal of one key, leaving one intact
rankWithKey :: Rankable key => [(key, b)] -> [[(key, b)]]
rankWithKey l = rank ( Data.List.map (\(k, b) -> (k, (k, b))) l)

-- ! Exercise 6.6.2
instance Rankable Int where
  rank = genericRank

instance Rankable Char where
  rank = genericRank

instance Rankable Integer where
  rank = genericRank

-- ! Exercise 6.6.3
instance Rankable Bool where
  rank k = [[t | (True, t) <- k], [f | (False, f) <- k]]

-- ! Exercise 6.6.4
instance (Rankable kеy1, Rankable kеy2) => Rankable (kеy1, kеy2) where
  rank = concatenate rank . rank . Data.List.map assoc where
    assoc :: ((k1, k2), a) -> (k1, (k2, a))
    assoc ((k1,k2),a) = (k1,(k2,a))

concatenate :: (Foldable t1, Foldable t2) => (a1 -> t2 a2) -> t1 a1 -> [a2]
concatenate f = Data.List.foldr (flip (Data.List.foldr (:)) . f) []

-- ! Exercise 6.6.5
instance (Rankable key) => Rankable (Maybe key) where
  rank k = ([n | (Nothing, n) <- k]) : rank [(Just key2, g) | (key2, g) <- k, isJust key2]

-- ! Exercise 6.6.6
instance (Rankable kеy) => Rankable [kеy] where
  rank = rank . Data.List.map (Data.Bifunctor.first uncons)
