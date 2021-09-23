module Uniq where
import Data.Set

-- ! exercise 3.5
uniq :: Eq a => [a] -> [a]
uniq [] = [] --Base Case
uniq (x:xs) = x : uniq (dropWhile (== x) xs)
