import Data.List


-- 1.1

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving Show

data ChessPiece = ChessPiece String (Pos -> [Pos])
-- Note: In the above solution the name of the type is the same as the name of its only constructor!

data Station a b = Machine [(a, Int)] -> b | Combine [Station a b]

-- 1.2

data IntTree = IntEmpty
             | IntNode Integer IntTree IntTree
             deriving Show

mapIntTree :: (Integer -> Integer) -> IntTree -> IntTree
mapIntTree _ IntEmpty        = IntEmpty
mapIntTree f (IntNode v l r) = IntNode (f v) (mapIntTree f l) (mapIntTree f r)

intTree2list :: IntTree -> [Integer]
intTree2list IntEmpty        = []
intTree2list (IntNode v l r) = v : intTree2list l ++ intTree2list r

instance Eq IntTree where
  IntEmpty == IntEmpty = True
  (IntNode v1 l1 r1) == (IntNode v2 l2 r2) = v1 == v2 && l1 == l2 && r1 == r2
  _ == _ = False

{- OR:

instance Eq IntTree where
  IntEmpty == IntEmpty = True
  -- Use pattern guards instead of &&
  IntNode x1 l1 r1 == IntNode x2 l2 r2
    | x1 == x2
    , l1 == l2
    , r1 == r2 = True
  _ == _ = False
-}


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty        = Empty
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)

instance Functor Tree where
  fmap = mapTree

tree2list :: Tree a -> [a]
tree2list Empty        = []
tree2list (Node a l r) = a : tree2list l ++ tree2list r

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f x = foldr f x . tree2list

instance Eq a => Eq (Tree a) where
  t1 == t2 = sameElems1 (tree2list t1) (tree2list t2)

sameElems1 :: Eq a => [a] -> [a] -> Bool
sameElems1 = same
  where same [] ys     = null ys
        same _  []     = False
        same (x:xs) ys | elem x ys = same xs (delete x ys)
                       | otherwise = False

-- Without explicitly walking through the lists
sameElems2 :: Eq a => [a] -> [a] -> Bool
sameElems2 l1 l2 = and bs && null rest
  where (rest, bs) = mapAccumL f l1 l2
        f l e      = (delete e l, elem e l)

-- Using (\\)
sameElems3 :: Eq a => [a] -> [a] -> Bool
sameElems3 l1 l2 = null (l1 \\ l2) && null (l2 \\ l1)

-- Using sort, which requires (Ord a)
sameElems4 :: Ord a => [a] -> [a] -> Bool
sameElems4 l1 l2 = sort l1 == sort l2

-- Extra
tree2listBF :: Tree a -> [a]
tree2listBF t = go [t]
  where go []                = []
        go (Empty : ts)      = go ts
        go (Node x l r : ts) = x : go (ts ++ [l, r])
