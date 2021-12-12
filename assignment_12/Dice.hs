module AST where

import System.Random
import RandomState

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
          -- adding and removing -/+
          | Add Expr Expr | Rem Expr Expr
  deriving (Show)

infixl 6 :+: 

type DiceAction m = Integer -> m Integer

evalM :: Monad m => Expr -> (Integer -> m Integer) -> m Integer

-- Lit just returns the factual number
evalM (Lit intg) inp = return intg

-- The actual dice
evalM (Dice intg) inp = inp intg

-- if we want max (max max max super max max max)
evalM (Max x y) inp = do
  -- first inp
  left <- evalM x inp
  -- second inp
  right <- evalM y inp
  -- actual output
  return (max left right)

-- if we want min
evalM (Min x y) inp = do
  -- first inp
  left <- evalM x inp
  -- second inp
  right <- evalM y inp
  --actual output
  return (min left right)

-- if we want :+:
evalM (x :+: y) inp = do
  -- first inp
  left <- evalM x inp
  -- second inp
  right <- evalM y inp
  -- actual output
  return (left + right)

-- If we want to add
evalM (Add a b) da = do
  --first inp
  left <- evalM a da
  -- second inp
  right <- evalM b da
  -- actual output
  return (left `div` right)

-- If we want to remove
evalM (Rem x y) inp = do
  -- first inp
  left <- evalM x inp
  -- second inp
  right <- evalM y inp
  -- actual output
  return (left - right)

evalRIO :: Expr -> IO Integer
-- interactively ask the user to enter the result of a physical, actual dice roll
evalRIO inp = evalM inp (\x -> randomRIO(1,x) >>= userinp) 

-- our helper function
userinp :: Show b => b -> IO b
userinp a = do 
  putStr "you have rolled: ";
  print a ;
  return a

--evalM :: Expr -> DiceAction IO -> IO Integer             -- prototype
--evalM :: (Monad m) => Expr -> DiceAction m -> m Integer  -- final version

--evalRIO :: Expr -> IO Integer
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
--  where report x = do { putStr "rolled a "; print x; return x }

--evalIO :: Expr -> IO Integer

--evalND :: Expr -> [Integer]

avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

--expectation :: (Fractional a) => Expr -> a
--expectation e = avg (evalND e)

--evalR :: Expr -> RandomState Integer

--observed :: (Fractional a) => Int -> Expr -> IO a
