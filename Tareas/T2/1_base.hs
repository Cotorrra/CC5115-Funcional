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
foldF :: (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (Variable -> a) -> (Bool -> a) -> Formula -> a
foldF imp an no v c (Const bool)  = c bool
foldF imp an no v c (Var var)     = v var
foldF imp an no v c (Not f)       = no (foldF imp an no v c f)
foldF imp an no v c (And f1 f2)   = an (foldF imp an no v c f1) (foldF imp an no v c f2)
foldF imp an no v c (Imply f1 f2) = imp (foldF imp an no v c f1) (foldF imp an no v c f2)

-- Parte (b)
eval :: Formula -> Valuation -> Bool
eval f val = foldF (\f1 f2 -> f1 <= f2) (\f1 f2 -> f1 && f2) (\f1 -> not f1) (\v -> case (find v val) of 
                                                                                        Right b -> b 
                                                                                        Left err -> error err) (\c -> c) f

fvar :: Formula -> [Char]
fvar f = rmdups (foldF (\f1 f2 -> f1 ++ f2) (\f1 f2 -> f1 ++ f2) (\f1 -> f1) (\v -> [v]) (\_ -> []) f)


-- Parte (c)
isTaut :: Formula -> Maybe Valuation
isTaut f | null falses = Nothing
         | otherwise = Just (snd (head falses))
         where falses = filter (not . fst) [(eval f v, v) | v <- allVals f] 
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

allVals :: Formula -> [Valuation]
allVals f = map (zip vars) vals where
  vars = rmdups (fvar f)
  vals = bools (length vars)



{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

data Peg = L | C | R deriving (Eq, Show)
type Disk = Int
type Conf = Peg -> [Disk]
type Move = (Peg,Peg)

instance Show Conf where
  show c = show (c L, c C, c R)

-- Funciones auxs:

pop :: Peg -> Conf -> Conf
pop p c x | x == p = drop 1 (c x)
          | otherwise = c x

push :: Disk -> Peg -> Conf -> Conf
push d p c x | x == p = [d] ++ (c x)
             | otherwise = (c x) 

-- Parte (a)
-- Descomente el tipo y agregue su definición


step :: Move -> Conf -> Conf
step (s,t) c p | (c s) == [] = error "Trying to move from empty peg"
               | not (null (c t)) && (head (c s) > head (c t)) = error "Trying to move to a smaller peg"
               | p == s = drop 1 (c p)
               | p == t = [disk] ++ (c p)
               | otherwise = c p
               where disk = head (c s)


compPeg :: (Peg, Peg) -> Peg
compPeg (p1,p2) = head (filter (\x -> (x /= p1) && (x /= p2)) [C,R,L])

-- Parte (b)
optStrategy :: Int -> Move -> Conf -> [(Move,Conf)]
optStrategy 1 move c = [(move , (step move c))]
optStrategy n move@(s,t) c = (optStrategy (n-1) (s, compPeg move) c) ++ [(move, (step move c1))] ++ (optStrategy (n-1) (compPeg move, t) c2)
                            where c1 = (snd . last) (optStrategy (n-1) (s, compPeg move) c) 
                                  c2 = step move c1


makeInit :: Int -> Peg -> Conf
makeInit n p p' | p' == p   = [1..n]
                | otherwise = []


play :: Int -> Peg -> Peg -> IO()
play n s t = putStr $ show initConf ++ foldr f v (optStrategy n (s,t) initConf) where
  initConf  = makeInit n s
  v         = []
  f (m,c) r = "\n -> " ++ show m ++ " -> " ++ show c ++ r


isValidConf :: Conf -> Bool
isValidConf c = isSorted (c L) && isSorted (c R) && isSorted (c C)  

isSorted :: [Disk] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (d:e:ds) = (d < e) && (isSorted (e:ds))

isValid :: [Bool] -> Bool
isValid [] = True
isValid (False:_) = False
isValid (True:xs) = isValid xs

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
testoptStrategy = describe "Optimal strategy for Hanoi Tower:" $ modifyMaxSize (const 4) $ do
  it "Configuraciones generadas son validas" $
    property $ \n (s,t) -> 1 <= n ==> isValid (map (isValidConf . snd) (optStrategy (n::Int) (s,t) (makeInit n s)))
  it "Tamaño de la estrategia optima" $
    property $ \n (s,t) -> 1 <= n ==> 2 ^ (n::Int) - 1 == length (optStrategy n (s,t) (makeInit n s))


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

sumsqr :: Nat -> Nat
sumsqr = snd . sumsqrxs

-- Descomente el tipo y agregue su definición
sumsqrxs :: Nat -> (Nat, Nat)
sumsqrxs = foldNat f v where
    v = (Zero, Zero)
    f (n, r) = (Succ n, add r (mult (Succ n) (Succ n))) 


{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
data BinTree a = Leaf a | InNode (BinTree a) a (BinTree a)

foldBT :: (b -> a -> b -> b) -> (a -> b) -> (BinTree a -> b)
foldBT f g (Leaf v)         = g v
foldBT f g (InNode t1 v t2) = f (foldBT f g t1) v (foldBT f g t2)

-- Parte (a)
{-
Sea:
- f  :: b -> a -> b -> b 
- g  :: a -> b , 
- f' :: b' -> a -> b' -> b'
- g' :: a -> b' 
- h  :: b -> b' 
Tales que:
  1. h (g v) = g’ v
  2. h (f x1 v x2) = f’ (h x1) v (h x2)
  Probar que h . foldBT f g = foldBT f’ g’ .

  Caso base:
    - Leaf v
      (h . (foldBT f g)) Leaf v
      = h (foldBT f g (Leaf v))
      = h (g v)
      = g' v

  Caso recursivo:
    Hipótesis inductiva:
      - h (foldBT f g t1) = foldBT f' g' t1
      - h (foldBT f g t2) = foldBT f' g' t2
     
    InNode t1 v t2:
      (h . (foldBT f g)) (InNode t1 v t2)
      = h (foldBT f g (InNode t1 v t2))              
      = h (f (foldBT f g t1) v (foldBT f g t2))       -- def (2) de foldBT
      = f' (h (foldBT f g t1)) v (h (foldBT f g t2))  -- prop de h
      = f' (fold f' g' t1) v (fold f' g' t2)          -- Hipotesis inductiva
      = foldBT f' g' (InNode t1 v t2)                 -- def (2) de foldBT
-}

-- Parte (b)
flattenBT :: BinTree a -> [a]
flattenBT = foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[])

sizeBT :: BinTree a -> Int
sizeBT = foldBT (\r1 v r2 -> r1 + 1 + r2) (const 1)

{-
Probar length . falttenBT = sizeBT
  Para poder utilizar a), primero hay que verificar que
  encontrar h, g, g', f y f' cumplan con:
    1. h (g v) = g’ v
    2. h (f x1 v x2) = f’ (h x1) v (h x2)

  Veamos si 
    - h = length :: [a] -> Int
    - g  = \x -> [x] :: a -> [a]  --- es equivalente a (:[])
    - g' = \x -> 1   :: a -> Int  --- es equivalente a (const 1)
    - f  = (\r1 v r2 -> r1 ++ [v] ++ r2) :: [a] -> a -> [a] -> [a]
    - f' = (\r1 v r2 -> r1 + 1 + r2)     :: Int -> a -> Int -> Int
    cumplen con las hipotesis necesarias para usar la parte a):

    1. Sea v :: a
    - h (g v) = length ((\x -> [x]) v) = length [v] = 1
    Por otro lado:
    - g' v = (\x -> 1) v = 1
    => h (g v) = g' v

    2. Sea x1 :: [a], v :: a y x2 :: [a]
    - h (f x1 v x2) = lenght ((\r1 v r2 -> r1 ++ [v] ++ r2) x1 v x2)
      = lenght (x1 ++ [v] ++ x2) = (lenght x1) + 1 + (lenght x2)  * (usando el hint)
    Por otro lado:
    - f' (h x1) v (h x2) = (\r1 v r2 -> r1 + 1 + r2) (lenght x1) v (lenght x2)
    = (lenght x1) + 1 + (lenght x2)

    Ya que se verificaron los hipotesis, se utiliza la parte a) para concluir
    => lenght . falttenBT 
      = lenght . (foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[])) 
      = h . (foldBT f g)
      = foldBT f' g'
      = foldBT (\r1 v r2 -> r1 + 1 + r2) (const 1)
      = sizeBT
-}


-- Parte (c)
mirrorBT :: BinTree a -> BinTree a
mirrorBT = foldBT (\r1 v r2 -> InNode r2 v r1) Leaf

idBT :: BinTree a -> BinTree a
idBT = foldBT InNode Leaf

{-
Probar mirrorBT . mirrotBT = idBT
  Para poder utilizar a), primero hay que verificar que
  encontrar h, g, g', f y f' cumplan con:
    1. h (g v) = g’ v
    2. h (f x1 v x2) = f’ (h x1) v (h x2)

  Veamos si 
    - h  = \x -> (foldBT (\r1 v r2 -> InNode r2 v r1) Leaf) x -- :: BinTree a -> BinTree a
    - g  = \x -> Leaf x :: a -> BinTree a 
    - g' = \x -> Leaf x :: a -> BinTree a
    - f  = (\r1 v r2 -> InNode r2 v r1)   :: BinTree a -> a -> BinTree a -> BinTree a
    - f' = (\r1 v r2 -> InNode r1 v r2)   :: BinTree a -> a -> BinTree a -> BinTree a
  cumplen con las hipotesis necesarias para usar la parte a):

    1. Sea v :: a
    - h (g v) = mirrorBT (Leaf v) 
      = (foldBT (\r1 v r2 -> InNode r2 v r1) Leaf) (Leaf v)
      = (Leaf) v -- Usando la definición 1 de foldBT
      = Leaf v
    Por otro lado:
    - g' v = (Leaf) v = Leaf v
    Así h (g v) = g' v
    
    2. Sea x1 :: BinTree a, v :: a y x2 :: BinTree a
    - h (f x1 v x2) = mirrorBT ((\r1 v r2 -> InNode r2 v r1) x1 v x2)
      = mirrorBT (InNode x2 v x1)
      = (foldBT (\r1 v r2 -> InNode r2 v r1) Leaf) (InNode x2 v x1)
      = (\r1 v r2 -> InNode r2 v r1) (foldBT f g x2) v (foldBT f g x1) -- definicion 2 de foldBT
      = InNode (foldBT f g x1) v (foldBT f g x2)
    Por otro lado:
    - f’ (h x1) v (h x2) = (\r1 v r2 -> InNode r1 v r2) (mirrorBT x1) v (mirrorBT x2)
    = (\r1 v r2 -> InNode r1 v r2) 
        ((foldBT (\r1 v r2 -> InNode r2 v r1) Leaf) x1)
        v
        ((foldBT (\r1 v r2 -> InNode r2 v r1) Leaf) x2) 
    = (\r1 v r2 -> InNode r1 v r2) (foldBT f g x1) v (foldBT f g x2) -- f = (\r1 v r2 -> InNode r2 v r1)
    = InNode (foldBT f g x1) v (foldBT f g x2)

    Ya que se verificaron los hipotesis, se utiliza la parte a) para concluir
    => mirrorBT . mirrorBT 
      = mirrorBT . (foldBT (\r1 v r2 -> InNode r2 v r1) (Leaf)) 
      = h . (foldBT f g)
      = foldBT f' g'
      = foldBT (\r1 v r2 -> InNode r1 v r2) (Leaf)
      = idBT

-}

-- Parte (d)
mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f (Leaf v)         = Leaf (f v)
mapBT f (InNode t1 v t2) = InNode (mapBT f t1) (f v) (mapBT f t2)

{-
Probar que: map f (flattenBT t) = flattenBT (mapBT f t)

Caso base: t = (Leaf l)
    map f (flattenBT (Leaf l))
    = map f [l]
    = [f l]
    Por otro lado:
    flattenBT (mapBT f (Leaf l))
    = flattenBT (Leaf (f l))
    = [f l]
Caso inductivo:
  HI: sea t1, t2 tales que: 
    - map f (flattenBT t1) = flattenBT (mapBT f t1)
    - map f (flattenBT t2) = flattenBT (mapBT f t2)
  Sea t = (InNode t1 v t2)
    map f (flattenBT (InNode t1 v t2))
    = map f ((flattenBT t1) ++ [v] ++ (flattenBT t2))
    = (map f (flattenBT t1)) ++ (map f [v]) ++ (map f (flattenBT t2))
    = flattenBT (mapBT f t1) ++ [f v] ++ flattenBT (mapBT f t2)
  Por otro lado:
    flattenBT (mapBT f (InNode t1 v t2))
    = flattenBT (InNode (mapBT f t1) (f v) (mapBT f t2))
    = foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (InNode (mapBT f t1) (f v) (mapBT f t2))
  
    = (\r1 v r2 -> r1 ++ [v] ++ r2) 
        (foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (mapBT f t1)) --- = (flattenBT (mapBT f t1))
        (f v)
        (foldBT (\r1 v r2 -> r1 ++ [v] ++ r2) (:[]) (mapBT f t1)) --- = (flattenBT (mapBT f t2))
  
      = (\r1 v r2 -> r1 ++ [v] ++ r2) (flattenBT (mapBT f t1)) (f v) (flattenBT (mapBT f t2))
    = (flattenBT (mapBT f t1)) ++ [f v] ++ (flattenBT (mapBT f t2))
  
  Así: map f (flattenBT t) = flattenBT (mapBT f t).  

-}



main :: IO()
main = hspec testoptStrategy
