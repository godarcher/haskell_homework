module ParseDice where

import Control.Applicative
import Parser
import Dice
import Data.List

{-

expr     = fraction

fraction = formula
         | term, "/", positive

formula  = formula, "+", term
         | formula, "-", term
         | term

term     = "(", expr, ")"
         | integer                  -- constants
         | [positive], "d", positive -- dice

positive = <an integer greater than 0>

-}

expr :: Parser Expr
expr = fraction


fraction :: Parser Expr
fraction = do { f <- term; symbol "/"; p <- positive; return (f :/: p)}
  <|> formula

formula :: Parser Expr
formula =
      do { t <- term; symbol "+"; a <- formula; return (t :+: a)}
  <|> do { t <- term; symbol "-"; a <- formula; return (t :-: a)}
  <|> term

term :: Parser Expr
term = symbol "(" *> expr <* symbol ")"
  <|> do { n <- positive; symbol "d"; r <- positive; return (builder n r)} 
  <|> do { symbol "d"; r <- positive; return (Dice r)} 
  <|> Lit <$> integer   

positive :: Parser Integer
positive = do {i <- integer; if i > 0 then return i else empty}

--Helper function to add the n dice throws together
builder :: Integer -> Integer -> Expr
builder 1 d = Dice d
builder n d = Dice d :+: builder (n-1) d

-- test cases: a list of tuples listing the input and output of "parseAll expr"
-- in case you used a different constructor for division, edit the definition of "</>"
test :: [(String, Maybe Expr)]
test = [ ""          =-> Nothing
       , "2"         =-> Just $ Lit 2
       , "d6"        =-> Just $ Dice 6
       , "(d6)"      =-> Just $ Dice 6
       , "((d6))"    =-> Just $ Dice 6
       , "2d10"      =-> Just $ Dice 10 :+: Dice 10
       , "xkcd"      =-> Nothing
       , "d6+d8"     =-> Just $ Dice 6 :+: Dice 8
       , "d10-1"     =-> Just $ Dice 10 :-: Lit 1
       , "1+d2+d3"   =-> Just $ Lit 1 :+: Dice 2 :+: Dice 3
       , "6-5-4"     =-> Just $ Lit 6 :-: Lit 5  :-: Lit 4
       , "2/d6"      =-> Nothing
       , "1+2/3"     =-> Nothing
       , "d6/2"      =-> Just $ Dice 6 </> 2  -- if these don't compile, edit below!
       , "1+(2/3)"   =-> Just $ Lit 1 :+: (Lit 2 </> 3)
       , "(1+2)/3"   =-> Just $ (Lit 1 :+: Lit 2) </> 3
       ]
  where (=->) = (,)
        infixr 0 =->

        -- depending on how you defined division in "Expr", choose the right definition here:
        (</>) :: Expr -> Integer -> Expr
        (</>) = (:/:)             -- data Expr = ... | Expr :/: Int | ...
        --(</>) = Div             --             ... | Div Expr Int | ...
        --x </> y = x :/: Lit y   --             ... | Expr :/: Expr | ...
        --x </> y = Div x (Lit y) --             ... | Div Expr Expr | ...

-- use this function to get all incorrect answers
deviations :: (Eq b) => (a->b) -> [(a,b)] -> [(a,b,b)]
deviations f ans = [ (x,y,f x) | (x,y) <- ans, f x /= y ]

-- combine with the functions in dice
calculate :: (Fractional a) => String -> Maybe a
calculate str = expectation <$> parseAll expr str
