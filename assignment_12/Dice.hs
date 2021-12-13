module AST where

import System.Random
import RandomState
import RandomGen
import Data.List
import Control.Monad

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
          | Expr :-: Expr | Div Expr Expr
  deriving (Show)

infixl 6 :+:
infixl 6 :-: 

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

-- If we want to devide
evalM (Div a b) da = do
  --first inp
  left <- evalM a da
  -- second inp
  right <- evalM b da
  -- actual output
  return (left `div` right)

-- If we want to remove
evalM (x :-: y) inp = do
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

evalND :: Expr -> [Integer]
evalND e = evalM e f
  where f n = unfoldr (\x -> if x == n+1 then Nothing else Just (x, x+1)) 1



avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)

evalR :: Expr -> RandomState Integer
evalR e = evalM e f2
  where f2 x = do
          gen <- get
          --Use randomR with a stdGen genarator to generate a tuple but only return the random value
          let (num, r) = randomR (1,x) gen
          put r
          return num

observed :: (Fractional a) => Int -> Expr -> IO a
observed n e = do 
  --This instead of a helper function I could not get to work
  help <- foldM f4 0 (f3 n e)
  --help returned the sum of all the rolls, so divide that by the amount of rolls
  return (fromIntegral help / fromIntegral n)
    --Roll one dice as long as x < n 
    where f3 n e = do 
            unfoldr (\x -> if x == n then Nothing else Just (evalRIO e, x+1)) 0
          --Add all the values rolled using fold for monads
          f4 num list = do
            listVals <- list
            return (num + listVals)
