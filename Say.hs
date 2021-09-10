module Say where


say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 14 = "fourteem"
say 15 = "fifteen"
say 16 = "sixteen"
say 17 = "seventeen"
say 18 = "eighteen"
say 19 = "nineteen"
say 20 = "twenty"
say 30 = "thirty"
say 40 = "fourty"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"
say x 
    | x < 100 = say(x -(mod x 10)) ++" "++ say(mod x 10)
    | x < 1000 = say((div x 100)) ++ " hundred and " ++ say(mod x 100)
    | x < 10000 = say ((div x 1000)) ++ " thousand " ++ say(mod x 1000)
    | x < 100000 = say((div x 1000)) ++ say(mod x 10000)
    | x < 1000000 = say((div x 100000)) ++ " hundred " ++ say(mod x 100000)
