module Char where

import Data.Char

-- !Exercise 2.5.1
--Returnss true when lower of a equals Lower of b
(~~) :: String -> String -> Bool
a ~~ b = map toLower a == map toLower b 

--reverseCase :: String -> String
--TODO make mape of char variant

--reverseCaseC :: Char -> Char 
a = toUpper(a)
--TODO FINISH THIS

--shift :: Int -> Char -> Char

--caesar :: Int -> String -> String

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
