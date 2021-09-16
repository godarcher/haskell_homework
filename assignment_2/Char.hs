module Char where

import Data.Char

-- !Exercise 2.5.1
--Returnss true when lower of a equals Lower of b
(~~) :: String -> String -> Bool
a ~~ b = map toLower a == map toLower b

-- !Exercise 2.5.2
reverseCase :: String -> String
reverseCase = map reverseCaseC

reverseCaseC :: Char -> Char
reverseCaseC a = if isLower a then toUpper a else toLower a

-- !Exercise 2.5.3
shift :: Int -> Char -> Char
shift x y 
    | fromEnum y + x <= 90 = toEnum(fromEnum y + x)
    | otherwise =  if fromEnum y -25 + x >= 90 then shift (x-25) y else toEnum(fromEnum y -25 + x)



msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
