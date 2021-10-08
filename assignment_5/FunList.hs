module FunList where

compose :: [a -> a] -> (a -> a)
compose [] y = y
compose (x:xs) y = x (compose xs y)

compose' :: [a -> a] -> a -> a
compose' = foldl (flip (.)) id

foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

-- ! explanation of what foo does
-- foo multiplies the set 1...n, starting with 1
-- for n = 5, this gives 1 * 2 * 3 * 4 * 5 
-- = 2 * 3 * 20 = 120
-- ! explanation of how foo does this
-- it simply takes the set (1...n) and then
-- it uses compose with as function (mapping * over 1...n)
-- and as x just the integer 1, which makes use just stack
-- f1(f2(f3(x))) where x = 1 and function * so we basicly get:
-- (1*(2*(3*(4*(5*(1)))))), which results in 120

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' step empty = 