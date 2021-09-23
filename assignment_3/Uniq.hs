module Uniq where

--uniq :: (Eq a) => [a] -> [a]
uniq [] = [] --Base Case
uniq [a] = [a] --Singleton case
uniq (x:xs) = [10] --TODO: use group here for more then 1 element case???
