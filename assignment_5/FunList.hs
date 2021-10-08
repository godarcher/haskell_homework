module FunList where

compose :: [a -> a] -> (a -> a)
compose [] y = y
compose (x:xs) y = x (compose xs y)

compose' :: [a -> a] -> a -> a
compose' = foldl (flip (.)) id

foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1
-- ! explanation of foo
-- foo 1 = 1, foo 2 = 2, foo 3 = 6, foo 4 = 24, foo 5 = 120
-- foo = input number * result of previous iteration
-- so foo 5 = 5 * 24 = 120