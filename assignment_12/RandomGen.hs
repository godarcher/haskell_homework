module RandomGen where

import Control.Monad
import System.Random
import RandomState

genRandInteger :: (Integer,Integer) -> RandomState Integer
genRandInteger (x,y) = do
  -- get seed
  seed <- get
  -- use seed to generate new 
  let (out, generated) = genme (x,y) seed
  put generated
  -- return output
  return out

-- actual logic is inside the helper function
genme :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
genme (x, y) = randomR(x, y)


roll_2d6 :: RandomState Integer
roll_2d6 = do
  a <- genRandInteger (1,6)
  b <- genRandInteger (1,6)
  return (a+b)


safeR :: RandomState a -> IO a
--safeR :: Control.Monad.IO.Class.MonadIO m => RandomState b -> m b
safeR mon = do
  -- get old sate
  oldstate <- getStdGen
  let (out, newstate) = runState mon oldstate
  -- set new state
  setStdGen newstate
  -- return output
  return out


--these definitions can be used to test your function a bit more thoroughly
randomN :: (Integer,Integer) -> Int -> StdGen -> [Integer]
randomN (a,b) n g = result
  where (result, _) = runState (replicateM n (genRandInteger (a,b))) g

testme :: [Integer]
testme = randomN (0,999) 100 (mkStdGen 42)
