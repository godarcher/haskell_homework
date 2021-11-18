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
-- Convert a stream into a list
tolist :: Stream a -> [a]
tolist (a :> x) = a : tolist x

-- Cycle converts lists to streams
cycle :: [a] -> Stream a
cycle xs = foldr (:>) (cycle xs) xs

-- ! Exercise 9.4.5
nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

-- | The stream of prime numbers.
prime :: Stream Integer
prime = cycle primes where
        primes = [a | a <- [2..], checkPrime a]
        checkPrime a = null [d | d <- [2..a-1], a `mod` d == 0]

--primetwins :: Stream (Integer,Integer)

-- ! Exercise 9.4.6
--combine :: Stream a -> Stream a -> Stream a
