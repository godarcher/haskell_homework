module RandomGen where

import RandomState 

import System.Random 
import Control.Monad 

genRandInteger :: (Integer,Integer) -> RandomState Integer 
genRandInteger (a,b) = do 
  g <- get 
  let (x, g') = randomR (a,b) g
  put g' 
  return x

--or, much terser (and at this point maybe harder to understand) 
genRandInteger' :: (Integer,Integer) -> RandomState Integer
genRandInteger' (a,b) = St $ randomR (a,b) 

roll_2d6 :: RandomState Integer 
roll_2d6 = do
  a <- genRandInteger (1,6)
  b <- genRandInteger (1,6) 
  return (a+b)

safeR :: RandomState a -> IO a
safeR m = do 
  rng <- getStdGen 
  let (result, rng') = runState m rng 
  setStdGen rng'
  return result 

--these definitions can be used to test your function a bit more thoroughly 
randomN :: (Integer,Integer) -> Int -> StdGen -> [Integer]
randomN (a,b) n g = result
  where (result, _) = runState (replicateM n (genRandInteger (a,b))) g

testme :: [Integer] 
testme = randomN (0,999) 100 (mkStdGen 42) 
