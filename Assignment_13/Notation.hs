module Notation where

import Data.Time

siri :: IO ()
siri = 
  putStrLn "What is your name?" >>
  getLine >>= \name ->
  getZonedTime >>= \now ->
  putStrLn (name ++ formatTime defaultTimeLocale ", the time is %H:%M" now)

siri' :: IO ()
siri' = do
  putStrLn "What is your name?"
  name <- getLine
  now <- getZonedTime
  putStrLn (name ++ formatTime defaultTimeLocale ", the time is %H:%M" now)

mayLookup :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
mayLookup maybekey assocs = do
  key <- maybekey
  result <- lookup key assocs
  return result

mayLookup' :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
mayLookup' maybekey assocs =
  maybekey >>= \key -> lookup key assocs
  --Equivalent solutions:
  --maybekey >>= \key -> lookup key assocs >>= \result -> return result
  --maybekey >>= \key -> lookup key assocs >>= return
  --(maybekey >>= flip lookup assocs) >>= return
  --maybekey >>= flip lookup assocs
