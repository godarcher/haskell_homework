module Lego where

import Data.List
import Data.Tuple


-- ! Exercise 3.7.1
-- * If x is not null, so the length is bigger then drop the y'th element of x
removeAt :: Int -> [a] -> [a]
removeAt y x
    | not(null x) = take (y-1) x ++ take y (drop y x)


--sortWithPos :: (Ord a) => [a] -> [(a,Int)]

--sortedPos :: (Ord a) => [a] -> [(a,Int)]
