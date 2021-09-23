module Uniq where
import Data.Set

-- ! exercise 3.5
-- * We drop the next element if it is equal to the current element, hence only if they are next to each other
uniq :: Eq a => [a] -> [a]
uniq [] = [] --Base Case
uniq (x:xs) = x : uniq (dropWhile (== x) xs) --Recursive case
