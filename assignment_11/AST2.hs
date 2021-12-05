module AST where

-- this template uses infix constructors; feel free to use AST.hs (which uses infix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)
import Result

type Identifier = String

data Expr = Lit Integer | Var Identifier | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show)

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a 
eval (Lit k) _ = Okay (fromInteger k) 
eval (Add x y) vars = pure (+) <*> eval x vars <*> eval y vars

eval (Sub x y) vars = pure (-) <*> eval x vars <*> eval y vars

eval (Mul x y) vars = pure (*) <*> eval x vars <*> eval y vars

eval (Div x y) vars = case (eval x vars, eval y vars) of 
                     (Okay _, Okay 0)  -> Error ["division by zero"]
                     _ -> pure (/) <*> eval x vars <*> eval y vars

eval (Var name) vars = newLookup name vars

newLookup :: Identifier -> [(Identifier, b)] -> Result b
newLookup var [] = Error ["unkown variable: " ++ var]
newLookup var ((x,y):xs) 
    | var == x = Okay y
    | otherwise = newLookup var xs



