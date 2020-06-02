{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck(modifyMaxSize)



{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

type Assoc k v = [(k,v)]
type Error     = String




-- Parte (a)
find :: (Eq k, Show k, Eq v) => k -> Assoc k v -> Either Error v
find key table 
    | length result == 0 = Left ("Key " ++ (show key) ++ " not found")
    | length result == 1 = Right (head result)
    | otherwise = Left ("Multiple values for key " ++ (show key))
    where result = rmdups [v' | (k',v')<-table, k'==key]

-- Parte (b)
{-
- Se necesita Eq k para poder realizar el lookup en la tabla por 
  llaves igual a la dada. (El llamado a ==key dentro del lookup de la tabla)
- Se necesita Show k para mostrar errores al no encontrar llaves o haber 
  múltiples valores para ésta.
- El Eq v, se necesita para encontrar unicidad en los valores encontrados.
-}


{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

type Variable  = Char
data Formula   = Const Bool
               | Var Variable
               | Not Formula
               | And Formula Formula
               | Imply Formula Formula
type Valuation = Assoc Char Bool


-- Parte (a)
-- foldF ::
-- Agregue tipo y definicion


-- Parte (b)
-- eval :: Formula -> Valuation -> Bool
-- Descomente el tipo y agregue su definición

-- fvar :: Formula -> [Char]
-- Descomente el tipo y agregue su definición


-- Parte (c)
-- isTaut :: Formula -> Maybe Valuation
-- Descomente el tipo y agregue su definición

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
-- here I can also do x: filter (/= x) (rmdups xs)
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) r ++ map (True :) r where
  r = bools (n-1)

-- Cuando haya implementado fvar, puede descomentar allVals
{-
allVals :: Formula -> [Valuation]
allVals f = map (zip vars) vals where
  vars = rmdups (fvar f)
  vals = bools (length vars)
-}



{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

data Peg = L | C | R deriving (Eq, Show)
type Disk = Int
type Conf = Peg -> [Disk]
type Move = (Peg,Peg)

instance Show Conf where
  show c = show (c L, c C, c R)


-- Parte (a)
-- step :: Move -> Conf -> Conf
-- Descomente el tipo y agregue su definición


-- Parte (b)
-- optStrategy :: Int -> Move -> Conf -> [(Move,Conf)]
-- Descomente el tipo y agregue su definición

-- Una vez que haya implementado optStrategy
-- puede descomentar las siguientes dos funciones
{-
makeInit :: Int -> Peg -> Conf
makeInit n p p' | p' == p   = [1..n]
                | otherwise = []

play :: Int -> Peg -> Peg -> IO()
play n s t = putStr $ show initConf ++ foldr f v (optStrategy n (s,t) initConf) where
  initConf  = makeInit n s
  v         = []
  f (m,c) r = "\n -> " ++ show m ++ " -> " ++ show c ++ r
-}


-- Parte (c)
others :: Peg -> (Peg,Peg)
others L = (R,C)
others C = (L,R)
others R = (L,C)

instance {-# OVERLAPPING #-} Arbitrary Move where
  arbitrary = do
    s <- frequency [(1,return L), (1,return C), (1,return L)]
    t <- let (x1,x2) = others s in frequency [(1,return x1), (1,return x2)]
    return (s,t)

testoptStrategy :: Spec
testoptStrategy = describe "Optimal strategy for Hanoi Tower:" $ modifyMaxSize (const 10) $ do
  it "Configuraciones generadas son validas" $
                                  -- reemplace lo que sigue a ==> por su codigo
    property $ \n (s,t) -> 1 <= n ==> (n::Int) == n && ((s,t)::Move) == (s,t)
  it "Tamaño de la estrategia optima" $
                                  -- reemplace lo que sigue a ==> por su codigo
    property $ \n (s,t) -> 1 <= n ==> (n::Int) == n && ((s,t)::Move) == (s,t)




{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero     m = Zero
mult (Succ n) m = add m (mult n m)

foldNat :: (b -> b) -> b -> Nat -> b
foldNat f v Zero     = v
foldNat f v (Succ n) = f (foldNat f v n)

-- sumsqr :: Nat -> Nat
-- Descomente el tipo y agregue su definición




{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
data BinTree a = Leaf a | InNode (BinTree a) a (BinTree a)

foldBT :: (b -> a -> b -> b) -> (a -> b) -> (BinTree a -> b)
foldBT f g (Leaf v)         = g v
foldBT f g (InNode t1 v t2) = f (foldBT f g t1) v (foldBT f g t2)

-- Parte (a)
{-
Agregue aquí su prueba
-}

-- Parte (b)
flattenBT :: BinTree a -> [a]
flattenBT = foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[])

sizeBT :: BinTree a -> Int
sizeBT = foldBT (\r1 v r2 -> r1 + 1 + r2) (const 1)

{-
Agregue aquí su prueba
-}


-- Parte (c)
mirrorBT :: BinTree a -> BinTree a
mirrorBT = foldBT (\r1 v r2 -> InNode r2 v r1) Leaf

idBT :: BinTree a -> BinTree a
idBT = foldBT InNode Leaf

{-
Agregue aquí su prueba
-}


-- Parte (d)
mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f (Leaf v)         = Leaf (f v)
mapBT f (InNode t1 v t2) = InNode (mapBT f t1) (f v) (mapBT f t2)

{-
Agregue aquí su prueba
-}



main :: IO()
main = hspec testoptStrategy
