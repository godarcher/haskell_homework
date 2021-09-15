module PolyType where

f8 x y  = if x <= y then x else y

f9 x y  = not x || y

f10 x y
  | x == 0    = y
  | otherwise = x + y

f11 x y = get 0
  where get n = if n == 0 then x else y

-- !Exercise 2.4.1
-- f8 can be used on arguments of type string, it will then output the shortest string
-- f9 can not be used on arguments of type string, it expects a boolean input 
-- f10 can not be used on arguments of type string, it expects an integer input
-- f11 can be used on arguments of type string, however it will always output the first string given to it
-- so f8 and f11 can be used on arguments of type string

-- !Exercise 2.4.2
-- f9 and f10 are non polymorphic because they only work with one specific input type
-- ad hoc/overloading means different behaviour for different input types, so different instances of the code
-- because all polymorphic pieces do not display different behaviour per instance, f8 and f11 are parametric polymorphic