module AST where

--start with either:
data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr


--eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a

--eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
--eval :: Expr -> Integer

eval (Lit i) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Sub x y) = eval x - eval y
eval (Div x y) = eval x / eval y
