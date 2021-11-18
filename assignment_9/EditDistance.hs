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
-- We implemented the subfunction (Int, Int) inside the where clause
editDistance :: String -> String -> Int
editDistance x y = distArray ! (length x, length y)
  where
    distArray = listArray ((0, 0), (length x, length y)) [calcDist a b | a <- [0 .. length x], b <- [0 .. length x]]
    calcDist 0 b = b
    calcDist a 0 = a
    calcDist a b
      | x !! (a - 1) == y !! (b - 1) = distArray ! (a - 1, b - 1)
      | otherwise = 1 + minimum (map (distArray !) [(a , b - 1),
                                              (a - 1, b),
                                              (a - 1, b - 1)])