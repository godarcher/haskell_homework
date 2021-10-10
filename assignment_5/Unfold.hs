module Unfold where

import Data.List (unfoldr, (++))
import Data.Char ( Char, intToDigit )
import Prelude hiding (take,zip,(++))

bits :: Int -> [Int]
bits = unfoldr (\x -> if x==0 then Nothing else Just(mod x 2, div x 2))

-- ! normal zip
zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []
--[ (x, y) | x <- xs | y <- ys ]

first :: [a] -> [(Int, a)]
first [] = []
first x = second x 1

second :: [a] -> Int -> [(Int, a)]
second [] _ = []
second (x:xs) i = (i, x) : second xs (i+1)

index :: [(Int, a)] -> [a]
index [] = []
index ((i,x):xs) = x : index xs

primes = sieve [2..]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0 ]

-- ! normal primes
-- might be interesting: https://gist.github.com/jnape/4366618

helper :: [Integer]
helper = sieve [2..] where
           sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p > 0 ]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

myappend :: [a] -> [a] -> [a]
myappend [] x = x
myappend (x:xs) y = x : myappend xs y

(++) :: [a] -> [a] -> [a]
(++) x y = apo f(x,y)
    where
        f :: ([a],[a]) -> Either [a] (a,([a],[a]))
        f([],y) = Left y
        f(x:xs, y) = Right(x,(xs, y))


insert :: (Ord a) => a -> [a] -> [a]
insert y x = apo f(y,x)
  where
    f(y,[]) = Left [y]
    f(y, x:xs)
     | y>=x = Right (x,(y,xs))
     | otherwise = Left (y:x:xs)
