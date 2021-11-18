module Stream where

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (16::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

-- ! Exercise 9.4.1
-- Get only the first element
head :: Stream a -> a
head (h :> _) = h

-- Drop only the first element
tail :: Stream a -> Stream a
tail (_ :> t) = t

-- ! Exercise 9.4.2
-- Infinitely repeat the element
repeat :: a -> Stream a
repeat a = a :> repeat a

-- map function
map :: (a -> b) -> (Stream a -> Stream b)
map func (x :> xs) = func x :> map func xs

-- Fuse two streams, with function f
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x :> xs) (y :> ys) = f x y :> zipWith f xs ys

-- A pred is used to filter a stream
filter :: (a -> Bool) -> Stream a -> Stream a
filter a (x :> xs)
    | a x = x :> filter a xs
    | otherwise = filter a xs

-- ! Exercise 9.4.3 
-- The terminal gets stuck after printing an opening bracket.
-- It most likely got stuck in an infinite loop because it 
-- keeps on skipping element for infinity 

-- ! Exercise 9.4.4
--toList :: Stream a -> [a]

cycle :: [a] -> Stream a
cycle xs = foldr (:>) (cycle xs) xs
--nat, fib :: Stream Integer
--nat = 0 :> zipWith (+) nat (repeat 1)
--fib = 0 :> 1 :> zipWith (+) fib (tail fib)
fibS :: Stream Integer
fibS = 0 :> (1 :> (f 0 1))
    where f x y = (x+y) :> (f y (x+y))

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = cycle primeList
    where
        primeList = [x | x <- [2..], isPrime x]
        isPrime x = null [d | d <- [2..x-1], x `mod` d == 0]

--primetwins :: Stream (Integer,Integer)

--combine :: Stream a -> Stream a -> Stream a
