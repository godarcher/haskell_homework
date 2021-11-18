module EditDistance where

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1+distance xs (y:ys), 1+distance (x:xs) ys, cost x y+distance xs ys]

  cost x y = if x==y then 0 else 1

-- ! Version 1 of editdistance implemented
editDistance :: String -> String -> Int
editDistance x y = distArray ! (m, n)
  where
    distArray = listArray ((0, 0), (m, n)) [compute i j | i <- [0 .. m], j <- [0 .. n]]
    compute 0 j = j
    compute i 0 = i
    compute i j
      | x !! (i - 1) == y !! (j - 1) = distArray ! (i - 1, j - 1)
      | otherwise = 1 + minimum (map (distArray !) [(i , j - 1),
                                              (i - 1, j),
                                              (i - 1, j - 1)])
    m = length x
    n = length y
