module Obfuscate where

import Data.Char

-- ? Shuffle imports
import Control.Monad (replicateM)
import Data.Function (on)
import Data.List     (sortBy)


meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."

-- ! This function is made to generate the actual string list
toWords :: String -> [String]
toWords "" = []  -- * An empty string should result in an empty list
toWords s = buildWord s "" -- ? An non empty string should be converted

-- ! This function is made to build a word, it does all the hard work 
buildWord :: String -> String -> [String]
buildWord "" ""   = []     -- ? If there is no word, append nothing to list (emtpy set)
buildWord "" word = [word] -- ? If there is a word, append to list (singleton set)
buildWord (x:xs) word      -- ? In all other cases:
       | x == ',' || x == '.' = word : [x] : buildWord xs ""  -- * Handle punctuation marks seperately
       | x == ' '             = word : buildWord xs "" -- * skip whitespace
       | otherwise            = buildWord xs (word ++ [x]) -- * continue 

-- ! This function builds a text from scrambled words
-- cambridge :: String -> String
-- cambridge: map cambridgeWord buildWord meme

-- ! This function iterates over the words in the list
--cambridgeWord :: String -> String
--cambridgeWord "" = "" -- * Base case: Empty words stay empty words
cambridgeWord (x:xs) word -- ? In all other cases:
       | x == head word || x == last word = word : [x] : cambridgeWord xs ""
       | otherwise = word : [x] : cambridgeWord xs "" --TODO implement scrambling for these letters
       -- ! https://stackoverflow.com/questions/14692059/how-to-shuffle-a-list

-- A random seed to make results reproducable (scientific method)
seed :: Int
seed = 3

-- A randomly chosen, program-scoped constant from the range [1 .. 10]
randomInt :: Foldable t => Int -> t a -> Int
randomInt x y
       | (x * 2 + 1) < length y && (x * 2 + 1) >= 0 = x * 2 + 1
       | x == length y = x
       | x - 1 < length y && x - 1 >= 0 = x - 1

seed :: Int
seed = 3

-- Divides the list in half
divideList :: [a] -> ([a], [a])
divideList list = splitAt ((length list) `div` 2) list

randomizeList :: Eq a => [a] -> [a]
randomizeList list =
  let lists = (divideList list) in
    if (length list) > 1
    then if (randomInt > 5)
    then (randomizeList (fst lists) ++ randomizeList (snd lists))
    else (randomizeList (snd lists) ++ randomizeList (fst lists))
    else list
