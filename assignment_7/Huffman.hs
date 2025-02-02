module Huffman where

import Data.List
import Data.Function ( on )
import qualified Data.Map
import Stream

data Btree a = Tip a | Bin (Btree a) (Btree a)
  deriving Eq -- Show is done manually
data Bit = O | I
  deriving (Eq,Ord,Show)

-----------------------------------------------------------------------

-- Exercise 7.4
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies stri = Data.List.map (\x -> (head x, length x)) $ group $ sort stri
-----------------------------------------------------------------------

-- Exercise 7.5
-- our Huffman main function uses a variety of helper funtions
huffman :: [(a,Int)] -> Btree a
huffman = switch . builder

-- switch is an Converter
switch :: Btree (a, b) -> Btree a
switch (Tip (a, _)) = Tip a
switch (Bin a b) = Bin (switch a) (switch b)

heavyness :: Num b => Btree (a, b) -> b
heavyness (Tip (_,a)) = a
heavyness (Bin a b) = heavyness a + heavyness b

setup :: [Btree (a, Int)] -> Btree (a, Int)
setup[lis] = lis
setup (a:b:cs) = setup $  insertBy (compare `on` heavyness) (Bin a b) cs

builder :: [(a, Int)] -> Btree (a, Int)
builder = setup . Data.List.map (\(a,b) -> Tip (a,b)) . sorter

-- sort frequency on index in alphabet
sorter :: [(a,Int)] -> [(a,Int)]
sorter = sortBy (compare `on` snd)

-----------------------------------------------------------------------

-- Exercise 7.6

-- converts a character list into a bit sequenc
encode :: (Foldable t, Ord k) => Btree k -> t k -> [Bit]
encode a = concatMap (\b -> Data.Map.fromList (enc a) Data.Map.! b)

-- helper of below func
helpenc :: Btree a -> [Bit] -> [(a, [Bit])]
helpenc (Tip x) xs = [(x, xs)]
helpenc (Bin x y) xs = helpenc x (xs ++ [O]) ++ helpenc y (xs ++ [I])

-- create ass0ciatstion li
enc :: Btree a -> [(a, [Bit])]
enc (Tip x) = [(x, [O])]
enc (Bin x y) = sortBy (compare `on` length . snd) (helpenc x [O] ++ helpenc y [I])

-----------------------------------------------------------------------

--decode :: (Ord a) => Btree a -> [Bit] -> [a]
-----------------------------------------------------------------------

backus1978 :: String
backus1978 =
  "An alternative functional style of programming is founded on the\n\
  \use of combining forms for creating programs. Functional programs\n\
  \deal with structured data, are often nonrepetitive and nonrecursive,\n\
  \are hierarchically constructed, do not name their arguments, and do\n\
  \not require the complex machinery of procedure declarations to become\n\
  \generally applicable. Combining forms can use high level programs to build\n\
  \still higher level ones in a style not possible in conventional languages."

thanksForAllTheFish :: String
thanksForAllTheFish =
  "It is an important and popular fact that things are not always what\n\
  \they seem. For instance, on the planet Earth, man had always\n\
  \assumed that he was more intelligent than dolphins because he had\n\
  \achieved so much -- the wheel, New York, wars and so on -- whilst all\n\
  \the dolphins had ever done was muck about in the water having a\n\
  \good time. But conversely, the dolphins had always believed that they\n\
  \were far more intelligent than man -- for precisely the same reasons.\n\
  \\n\
  \Curiously enough, the dolphins had long known of the impending\n\
  \destruction of the planet Earth and had made many attempts to alert\n\
  \mankind of the danger; but most of their communications were\n\
  \misinterpreted as amusing attempts to punch footballs or whistle for\n\
  \tidbits, so they eventually gave up and left the Earth by their own\n\
  \means shortly before the Vogons arrived.\n\
  \\n\
  \The last ever dolphin message was misinterpreted as a surprisingly\n\
  \sophisticated attempt to do a double-backwards-somersault through\n\
  \a hoop whilst whistling the \"Star Sprangled Banner\", but in fact the\n\
  \message was this: So long and thanks for all the fish.\n\
  \\n\
  \In fact there was only one species on the planet more intelligent\n\
  \than dolphins, and they spent a lot of their time in behavioural\n\
  \research laboratories running round inside wheels and conducting\n\
  \frighteningly elegant and subtle experiments on man. The fact that\n\
  \once again man completely misinterpreted this relationship was\n\
  \entirely according to these creatures' plans."

-- a function which performs a rudimentary test to see if you get the correct frequencies for the above text
testFrequencies :: Bool
testFrequencies = (\x y->frequencies x == frequencies y) soLongAnd thanksForAllTheFish
  where soLongAnd = concatMap (\(c,n)->replicate n c) $ frequencies (thanksForAllTheFish)

-- a function which performs a rudimentary test to see if you get the correct Huffman tree for (frequencies "hello world")
testHuffman :: IO()
testHuffman = display mismatches
  where
  mismatches = filter (\(x,y)->huffman x /= y) testSet
  (==>) x y = (x,y) -- local syntactic sugar for tuples
  testSet = [ [(' ',1)]
            ==> Tip ' '
            , [(' ',1),('d',1)]
            ==> Bin (Tip ' ') (Tip 'd')
            , [(' ',1),('d',1),('e',1)]
            ==> Bin (Tip 'e') (Bin (Tip ' ') (Tip 'd'))
            , [(' ',1),('d',1),('e',1),('h',1)]
            ==> Bin (Bin (Tip 'e') (Tip 'h')) (Bin (Tip ' ') (Tip 'd'))
            , [(' ',1),('d',1),('e',1),('h',1),('r',1)]
            ==> Bin (Bin (Tip ' ') (Tip 'd')) (Bin (Tip 'r') (Bin (Tip 'e') (Tip 'h')))
            , [(' ',1),('d',1),('e',1),('h',1),('r',1),('w',1)]
            ==> Bin (Bin (Tip ' ') (Tip 'd')) (Bin (Bin (Tip 'r') (Tip 'w')) (Bin (Tip 'e') (Tip 'h')))
            , [(' ',1),('d',1),('e',1),('h',1),('r',1),('w',1),('o',2)]
            ==> Bin (Bin (Bin (Tip ' ') (Tip 'd')) (Tip 'o')) (Bin (Bin (Tip 'r') (Tip 'w')) (Bin (Tip 'e') (Tip 'h')))
            , [(' ',1),('d',1),('e',1),('h',1),('r',1),('w',1),('o',2),('l',3)]
            ==> Bin (Bin (Bin (Tip 'r') (Tip 'w')) (Bin (Tip 'e') (Tip 'h'))) (Bin (Tip 'l') (Bin (Bin (Tip ' ') (Tip 'd')) (Tip 'o')))
            ]
  display [] = do
    putStrLn "seems OK"
  display ((i,o):_) = do
    print i
    putStr "expected: " >> print o
    putStr "got: " >> print (huffman i)

-- to show Btree's with indentation, we define this manually
instance (Show a) => Show (Btree a) where
  show = indent "\n"
    where
    indent _   (Tip x)   = "Tip " ++ show x
    indent _   (Bin l@(Tip _) r@(Tip _))
                         = "Bin (" ++ show l ++ ") (" ++ show r ++ ")"
    indent pre (Bin l r) = "Bin (" ++ indent (pre++"     ") l ++ ")" ++ pre ++
                           "    (" ++ indent (pre++"     ") r ++ ")"

-- a semi-graphical pretty printer for Huffman trees
putTree :: (Show a) => Btree a -> IO ()
putTree = putStr . layout
  where
  layout :: (Show a) => Btree a -> String
  layout tree = go "" ("","","") tree
    where
    go pre (_,   _,   preN) (Tip k) = pre ++ preN ++ show k ++ "\n"
    go pre (preR,preL,preN) (Bin lt rt)
      = go (pre ++ preR) (hfill,v_bar,rbend) rt
        ++ (pre ++ preN) ++ junct ++
        go (pre ++ preL) (v_bar,hfill,lbend) lt

    junct = "┤\n" -- change to "+\n" if no Unicode
    hfill = "  "
    rbend = "┌-"  -- change to "/-" if no Unicode
    v_bar = "| "  -- change to "| " if no Unicode
    lbend = "└-"  -- change to "\\-" if no Unicode
