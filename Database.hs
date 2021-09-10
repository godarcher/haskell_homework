module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
ruben    =  ("Ruben",    22,  "Object Oriented Programming")

students :: [Person]
students = [elena, peter, pol, ruben]

age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (n,_,_) = n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_,_,n) = n

getPerson :: Person -> Person
getPerson x = x

showPerson :: Person -> String
showPerson (n,x,y) =  "my name is" ++ show n ++ "my age is" ++ show x ++ "my favorite course is" ++ show y ++ "."

twins :: Person -> Person -> Bool
twins (_,x,_) (_,y,_)
    | x == y    = True
    | otherwise = False

increaseAge :: Person ->  Person
increaseAge (x,y,z) = (x,y+1,z)

getAge :: [Age] -> [Age]
getAge x = x

-- age :: Person -> Age
-- age (_, n, _)  =  n +2 opdracht 1.5.6.1

--name :: Person -> Name
--name (n,_,_) = "dr" ++ n  opdracht 1.5.6.2
 
--filter (\n -> name n == "Frits") students opdracht 1.5.6.3

--filter (\a -> age a >= 20 && age a < 30) students 1.5.6.4

-- nog te doen 1.5.6.5

--filter (\s -> favouriteCourse s = "Functional Programming") students 1.5.6.6



