module Main where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO

{---------------------- functional parts -------------------------------}

type UserInput = [Int]
type Code = [Int]

--data Colour = ... 
options = 8
size  = 4

randomsol =
  do
    -- random initialization
    rand <- getStdGen
    -- 4 possible sizes
    let fullc = take 4 (randoms rand)
    -- 8 possible colors
    return (map ((+1) . (`mod` 8)) fullc)

-- main user io function
mainio :: IO UserInput
mainio =
  do
    -- user io
    putStr "Put in color "
    -- flush output
    hFlush stdout
    -- get input from userinput function
    inp <- getUi
    -- check if valid input and output if needed
    if (inp == [0]) || checker inp then return inp else (do putStrLn "input does not match conditions" mainio)

-- check if input satisfies conditions                                                      
checker :: UserInput -> Bool
-- we need two things, size and option conditions being satisfied
checker try = all d t try (length try == size)
                where d b = (b>0) && (b<=options)

-- convert an string to int
stringtoint :: String -> Int
stringtoint i =
  case reads i of
    [(a, "")] -> a
    -- everything else gives error (-1)
    _         -> -1

-- this function checks the amount of red matches (full matches)
redMatches :: Code -> UserInput -> [Int]
-- if either of the two is empty, return empty
redMatches [] _ = []
redMatches _ [] = []
-- else (both non empty, we compare them per element)
redMatches (a:as) (b:bs)
    |a == b = a : redMatches as bs
    |otherwise = redMatches as bs

-- calculate amount of non full matches, and then remove full matches from it		 
white solution guess = length (whiteMatch solution guess) - black solution guess


partialMatches in1 in2 = partialMatch (sort in1) (sort in2)

partialMatch :: Code -> UserInput -> [Int]
-- if either of the two is empty, return empty
partialMatch [] _ = []
partialMatch _ [] = []
-- else (both non empty, we compare them per element)
partialMatch (a:as) (b:bs)
  | a == b = a : partialMatch as bs
  | a < b = partialMatch as (b:bs)
  | a > b = partialMatch bs (a:as)

--scoreAttempt :: (Ord a) => [a] -> [a] -> ???
scoreAttempt code guess = error "implement me"

-- userInp function
getUi :: IO UserInput
getUi = do input <- getLine
if input == "quit"
    then return [0]
    else return (map stringtoint (words input))

-- Some test cases from: https://www.boardgamecapital.com/game_rules/mastermind.pdf
test1, test2, test3, test4 :: Bool
test1 = scoreAttempt [1,2,3,4,5 :: Int] [2,6,3,7,8 :: Int] == (1,1)
test2 = scoreAttempt [1,2,3,4,2 :: Int] [5,6,3,3,7 :: Int] == (1,0)
test3 = scoreAttempt [1,2,1,3,3 :: Int] [4,1,5,6,7 :: Int] == (0,1)
test4 = scoreAttempt [4,1,5,6,7 :: Int] [1,2,1,3,3 :: Int] == (0,1)

main :: IO ()
main = do
  -- generate sol
  sol <- randomsol
