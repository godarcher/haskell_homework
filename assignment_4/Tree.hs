module Tree where
import Control.Arrow (ArrowChoice(right))

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

english5 = [ "waite"
           , "lurve"
           , "ainos"
           , "glens"
           , "cyons"
           , "neifs"
           , "coth"]


{----------- exercise 4.3 -------------}

--leaves :: Tree a -> Int
--nodes  :: Tree a -> Int
--height :: Tree a -> Int
--elems  :: Tree a -> [a]
--isSearchTree :: (Ord a) => Tree a -> Bool

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool -- member "Sjaak" a (step 2 check of de waarde in de tree is)
member x Leaf = False
member x (Node a left right)
  | x == a = True
  | x < a = member x left
  | x > a = member x right

insert :: (Ord a) => a -> Tree a -> Tree a  -- a = foldr insert Leaf ["Sjaak", "HAHAH", "NEE"] (step 1 voer deze command uit voor inserten waardes in de tree)
insert x Leaf = Node x Leaf Leaf
insert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (insert x left) right
  | x> a = Node a left(insert x right)

--delete :: (Ord a) => a -> Tree a -> Tree a

fromList :: (Ord a) => [a] -> Tree a --insert een lijst in de tree
fromList = foldr insert Leaf

-- delete :: Ord a => a -> Tree a -> Tree a
-- delete x Leaf = Leaf
-- delete x t@(Node a left right) = if member x t
--   then 
--     if x > a then Node a left (delete x right) --if bigger delete from the right
--     else
--         if x < a then Node a (delete x left) right --if smaller delete from the left
--         else remove x t -- if correct size actually remove it
--   else t

-- remove :: Ord a => Tree a -> a -> Tree a
-- remove (Node a left right) 
--   | left == Leaf = right 
--   | right == Leaf = left
--   | otherwise = Node (mostLeft right) left (update right)

-- mostLeft :: Tree a -> a
-- mostLeft (Node x Leaf _) = x
-- mostLeft (Node x left _) = mostLeft left

-- update :: Ord a => Tree a -> Tree a
-- update Leaf = Leaf
-- update t@(Node a left right)
--   | a == mostLeft t = delete a t 
--   | a < mostLeft t = Node a left (update right)
--   | a > mostLeft t = Node a (update left) right
{----------- exercise 4.5 -------------}

--inOrder :: Tree a -> [a]
inorder :: (Ord a) => Tree a -> [a]
inorder Leaf = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

--fromAscList :: [a] -> Tree a
--breadthFirst :: Tree a -> [a]

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}

{-
layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree
  where
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  --go pre (_,_,preN) Leaf = pre ++ preN ++ "·\n" -- this explicitly draws the leaves
  --go _   _          Leaf = ""                   -- this vertically compresses the tree
  go pre _          Leaf = pre ++ "\n"            -- use more vertical space, but don't draw leaves
  go pre (preR,preL,preN) (Node k lt rt)
    = go (pre ++ preR) (hfill,v_bar,rbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preL) (v_bar,hfill,lbend) lt

  junct = "┤\n"         -- change to "+\n" if no Unicode
  hfill = fill ++ "  "
  rbend = fill ++ "╭─"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  lbend = fill ++ "╰─"  -- change to "\\-" if no Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)
-}
