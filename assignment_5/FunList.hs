module FunList where

compose :: [a -> a] -> (a -> a)
--compose [] =  
compose (x:xs) y = x(compose xs y)

--compose' :: [a -> a] -> (a -> a)

--foo :: (Integral n) => n -> n
--foo n = compose (map (*) [1..n]) 1

--foldr' :: (a -> b -> b) -> b -> [a] -> b
