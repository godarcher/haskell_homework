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


--shift :: Int -> Char -> Char

--caesar :: Int -> String -> String

input :: String -> String -> Bool
input x y  
    | x == y = True
    -- | otherwise = False if (x ~~ y) then True else False

reverseCase :: String -> String
reverseCase = map (\c -> if c >= 'a' && c <= 'z' then toUpper c else toLower c)


msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
