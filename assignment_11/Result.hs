module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
    fmap f (Okay x) = Okay (f x)
    fmap f (Error [x]) = Error [x]

--instance Applicative Result where
--  ...
