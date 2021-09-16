module Char where

import Data.Char

-- !Exercise 2.5.1
--Returns true when lower of a equals Lower of b
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

-- !Exercise 2.5.4
caesar :: Int -> Char -> Char
caesar x y 
    | y >= 'a' && y <= 'z' = shift x (toEnum(fromEnum y - 32)) --haalt 32 eraf wat ervoor zorgt dat hij naar de Uppercase ASCII gaat. Dus als je a invoert komt er A uit.
    | otherwise =  shift(x) y

-- !Exercise 2.5.5
-- If you decode the string with n = 21 and you decipher x = msg with n = 21, we get
-- FIRST I MUST SPRINKLE YOU WITH FAIRY DUST
msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
