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
genSolution :: IO [Int]
genSolution = do
  a <- roll_d6
  b <- roll_d6
  c <- roll_d6
  d <- roll_d6
  return [a, b, c, d :: Int]

-- ! Exercise 10.2.4 till 10.2.5
oneplay :: (Show t, Num t) => Code -> t -> IO ()
oneplay sol plays =
  do
    -- get userinput
    inp <- userInp

    -- check exit condition
    if inp == [0]
       then do putStrLn $ "The game has ended... The right answer was: " ++ show sol

       else do
              -- check userinput with resuld and give feedback based on it
              let result = scoreAttempt sol inp
              putStrLn (playfeedback result)

              -- check if game is won, and terminate it if needed
              if win result
                 then do putStrLn "You cracked the code, congratulations"
                 else oneplay sol (plays+1)
            where win(_,_,b) = b

-- gives feedback based on round
playfeedback :: (Int, Int, Bool) -> String
playfeedback (r,w,_) = show r ++ " full matches, " ++ show w ++ " partial matches"

userInp :: IO UserInput
userInp  =
  do
    -- ask for code
    putStr "please enter a code: "
    -- clean out
    hFlush stdout

    -- fetch user input
    inp <- getGuess

    -- check if invalid user input
    if (inp == [0]) || checker inp then return inp else (do putStrLn "input does not satisfy conditions"
                                                            userInp)
-- this function gets a users guess
getGuess :: IO UserInput
-- get userinput (ui)
getGuess = do ui <- getLine
              -- we could possibly make more stop keywords here
              if ui == "stop"
                 -- return zero list if stop, otherwise convert
                 then return [0]
                 else return (map stringtoint (words ui))

-- check if input is valid
checker :: UserInput -> Bool
-- we use constraints colors and width here
checker inp = all c inp && (length inp == 4)
                where c a = (a>0) && (a<=8)

-- convert an string to int
stringtoint :: String -> Int
stringtoint i =
  case reads i of
    -- remove empties from list
    [(a, "")] -> a
    -- everything else gives error (-1)
    _         -> -1

-- ! Exercise 10.2.6
main :: IO ()
main =
  do
    -- get solution
    sol <- genSolution 
    -- play game with solution
    oneplay sol 1               