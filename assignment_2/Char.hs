module Char where

import Data.Char

--(~~) :: String -> String -> Bool

--reverseCase :: String -> String

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
