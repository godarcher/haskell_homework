module DigitalSorting where

import Data.List
import Data.Bool
import Data.Maybe
import Data.Either

-- ! our extra input
import Data.Map
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
instance (Rankable key1, Rankable key2) => Rankable (key1, key2) where 
  rank = 