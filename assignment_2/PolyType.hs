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
-- f10 can not be used on arguments of type string, it expects an integer/float input
-- f11 can be used on arguments of type string, however it will always output the first string given to it
-- so f8 and f11 can be used on arguments of type string

-- !Exercise 2.4.2
-- f9 is non polymorphic cause it only works from bool --> bool --> bool
-- f10 is ad hoc polymorphic, you can not add everything into it, you need another implementation for strings or chars
-- f11 is parametric polymorphic, because we don't care about the type, it is the same for any type

-- ?F8 is ad hoc, but depends on the ord(a), is ord defined for everything? then parametric otherwise ad hoc