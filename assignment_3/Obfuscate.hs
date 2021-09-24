module Obfuscate where

import Data.Char

--cambridge :: String -> String

meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."

wordToken :: String -> [String]
wordToken "" = [] -- empty list
wordToken str = helper str "" -- start helper with empty current word
    where helper :: String -> String -> [String]
          -- when the entire string is consumed
          helper "" ""      = [] -- if no current word, append nothing
          helper "" current = [current] -- if current word, append this to the list
          -- otherwise
          helper (x:xs) current
              | x == ',' || x == '.' = current : [x] : helper xs "" -- add comma or fullstop as extra word
              | x == ' '             = current : helper xs "" -- but skip on whitespaces
              | otherwise            = helper xs (current ++ [x]) -- if no seperator, just continue building up the current word
