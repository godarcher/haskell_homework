module Lego where

import Data.List
import Data.Tuple


-- ! Exercise 3.7.1
-- * If x is not null, so the length is bigger then drop the y'th element of x
removeAt :: Int -> [a] -> [a]
removeAt y x
    | not(null x) = take (y-1) x ++ take y (drop y x)

-- ! Exercise 3.7.2 
-- * sort a given list but also rescord the original position
-- ? Thinking: 1 get list of indexes from string 2 zip original list with list of indexes (so that char is first for sorting) 3 sort
sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos x = sort(zip x [0..])

-- ! Exercise 3.7.3
-- * append index would have when list would have been sorted
-- ? input = "bac" 1: append original index "b0 a1 c2" 2: sort based on alphabet "a0 b1 c2"
-- ? 3: invert so orginal index is first "0a 1b 2c" 4: append new index "(1a, 0), (0b, 1), (2c, 2)"
-- ? 5: sort back to original index "(0b, 1), (1a, 0), (2c, 2)" 6: take out new positions (1, 0 , 2) 7: zip with original "(1b 0a 2c)"
sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos x = zip x (map snd two_index) where -- ! this code part takes two_index and zips the found indexes with x "(1, b), (0, a), (2,c)"
    two_index = sort(zip sort_old [0..]) -- ! This code part takes sort_old and returns "((0, b), 1), ((1, a), 0), ((2, c), 2)"
    sort_old = map swap(sort(zip x [0..])) -- ! This code part takes "bac" and returns "(1, a), (0, b), (2, c)"  (step 1 to 3)
