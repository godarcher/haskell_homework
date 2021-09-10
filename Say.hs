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
say  9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"
say 13 = "thirteen"
say 14 = "fourteen"
say 15 = "fifteen"
say 16 = "sixteen"
say 17 = "seventeen"
say 18 = "eighteen"
say 19 = "nineteen"
say 20 = "twenty"
say 30 = "thirty"
say 40 = "fourty"
say 42 = "the answer to life, the universe, and everything"
say 50 = "fifty"
say 60 = "sixty"
say 70 = "seventy"
say 80 = "eighty"
say 90 = "ninety"
say 100 = "hundred"
say 1000 = "thousand"
say x 
    -- We developed an algorithms that uses the fact that integers are rounded down to
    -- recurrently translate the number while using "REST" parts
     | x < 100 = say(x - mod x 10) ++ " " ++ say(x - (10 * div x 10)) 
     | x < 1000 = say(div x 100) ++ if (x-(100 * div x 100)) > 0 then " hundred " ++ say(x - (100 * div x 100)) else " hundred"
     | x <= 100000 = say(div x 1000) ++ if (x-(1000 * div x 1000)) > 0 then " thousand " ++  say(x - (1000 * div x 1000)) else " thousand"
     | x < 1000000 = say(div x 100000) ++ " hundred " ++ say(x - (100000 * div x 100000))

-- DEBUG MODE WHICH GIVES REST VALUES
-- say x 
--     | x < 100 = say(x - mod x 10) ++" " ++ say(x - (10 * (div x 10))) 
--     | x < 1000 = say((div sx 100)) ++ " hundred " ++ " REST2 " ++ show(x - (100 * (div x 100))) ++ say(x - (100 * (div x 100)))
--     | x < 10000 = say ((div x 1000)) ++ " thousand "   ++ " REST3 " ++ show(x - (1000 * (div x 1000))) ++ say(x - (1000 * (div x 1000)))
--     | x < 100000 = say((div x 1000))++ " thousand " ++ " REST4 " ++ show(x - (1000 * (div x 1000)))  ++ say(x - (1000 *  (div x 1000)))
--     | x < 1000000 = say((div x 100000)) ++ " hundred "  ++ " REST5 " ++ show(x - (100000 * (div x 100000))) ++ say(x - (100000 * (div x 100000)))

-- 434953
-- four hundred rest 34953
-- thirtyfour thousand rest 953
-- zero rest 9