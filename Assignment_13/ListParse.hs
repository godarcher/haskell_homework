{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
 -}

intList :: Parser [Integer]
intList = symbol "{" *> many natural <* symbol "}"

{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

intRecord :: Parser [Integer]
intRecord = do symbol "{" 
               n <- int
               symbol "#"
               ns <- replicateM (fromInteger n) (do {integer}) 
               symbol "}"
               return ns
