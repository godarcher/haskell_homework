module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt y x 
    | length x > 0 = ((take (y-1) x) ++ (take y (drop y x)))


--sortWithPos :: (Ord a) => [a] -> [(a,Int)]

--sortedPos :: (Ord a) => [a] -> [(a,Int)]
