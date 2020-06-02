
{-
    Estructuras de datos recursivas...
-}

data Nat = Zero | Succ Nat deriving (Eq, Show)

-- Embedding of Nat into Int
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- Sum of two Nats
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

data BinTree a = Leaf a | InNode (BinTree a) a (BinTree a) deriving (Eq, Show)

bt :: BinTree Int
bt = InNode (InNode (Leaf 1) 4 (Leaf 8)) 5 (Leaf 3)

flatten :: BinTree a -> [a] 
flatten (Leaf a) = [a]
flatten (InNode t1 v t2) = (flatten t1) ++ [v] ++ (flatten t2)

mirror :: BinTree a -> BinTree a
mirror (Leaf a) = (Leaf a)
mirror (InNode t1 v t2) = (InNode (mirror t2) v (mirror t1))

-- data Tree a = Lead a | 