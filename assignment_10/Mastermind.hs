-- module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO

{---------------------- functional parts -------------------------------}

-- ! Exercise 10.2.1
type UserInput = [Int]
type Code = [Int]

-- ! exercise 10.2.2

-- top function that does full calculation
--scoreAttempt :: Code -> UserInput -> (Int, Int, Bool)
scoreAttempt :: Code -> UserInput -> (Int, Int, Bool)
scoreAttempt sol inp = scoreAttempthelp (reds sol inp)
   where scoreAttempthelp bl = (bl, partials sol inp, bl == 4)

-- this function gets the full matches
reds :: Code -> UserInput -> Int
-- the non empty parts that match from sol and inp (thanks to emtpy discarding by helper)
reds sol inp = length (redMatches sol inp)

-- helper function for getting red matches
redMatches :: Code -> UserInput -> [Int]
-- if either of the two is empty, return empty
redMatches [] _ = []
redMatches _ [] = []
-- else (both non empty, we compare them per element)
redMatches (a:as) (b:bs)
    |a == b = a : redMatches as bs
    |otherwise = redMatches as bs

-- same idea as red, however we also substract number of full matches
partials :: Code -> UserInput -> Int
partials sol inp = length (partialMatches sol inp) - reds sol inp

-- get all partialmatches by sorting lists and comparing
partialMatches :: [Int] -> [Int] -> [Int]
partialMatches in1 in2 = partialMatch (sort in1) (sort in2)

-- get partialmatch
partialMatch :: Code -> UserInput -> [Int]
-- if either of the two is empty, return empty
partialMatch [] _ = []
partialMatch _ [] = []
-- else (both non empty, we compare them per element)
partialMatch (a:as) (b:bs)
  | a == b = a : partialMatch as bs
  | a < b = partialMatch as (b:bs)
  | a > b = partialMatch bs (a:as)


-- ! exercise 10.2.3

-- get 1 random color
roll_d6 :: IO Int
roll_d6 = randomRIO (1,8)

-- get list of random colors
generateSolution :: IO [Int]
generateSolution = do
  a <- roll_d6
  b <- roll_d6
  c <- roll_d6
  d <- roll_d6
  return [a, b, c, d :: Int]

-- randomsol =
--   do
--     -- random initialization
--     rand <- getStdGen
--     -- 4 possible sizes
--     let fullc = take 4 (randoms rand)
--     -- 8 possible colors
--     return (map ((+1) . (`mod` 8)) fullc)



-- -- main user io function
-- mainio :: IO UserInput
-- mainio =
--   do
--     -- user io
--     putStr "Put in color "
--     -- flush output
--     hFlush stdout
--     -- get input from userinput function
--     inp <- getUi
--     -- check if valid input and output if needed
--     if (inp == [0]) || checker inp then return inp else (do putStrLn "input does not match conditions" mainio)

-- check if input satisfies conditions                                                      
-- checker :: UserInput -> Bool
-- -- we need two things, size and option conditions being satisfied
-- checker try = all d t try (length try == 4)
--                 where d b = (b>0) && (b<=8)

-- convert an string to int
-- stringtoint :: String -> Int
-- stringtoint i =
--   case reads i of
--     [(a, "")] -> a
--     -- everything else gives error (-1)
--     _         -> -1

-- userInp function
--getUi :: IO UserInput
-- getUi = do input <- getLine
-- if input == "quit"
--     then return [0]
--     else return (map stringtoint (words input))

--main :: IO ()
--main = do
  -- generate sol
  --sol <- randomsol
