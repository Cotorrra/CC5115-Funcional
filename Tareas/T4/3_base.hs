import Control.Monad.State
import Data.Ratio
import Numeric.Probability.Distribution hiding (map,coin,filter)

{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}
type Matrix = [[Int]]
type Size = Int

sourceMatrix :: Matrix
sourceMatrix = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

targetMatrix :: Matrix
targetMatrix = [[14,16,15,13],[6,8,7,5],[2,4,3,1],[10,12,11,9]]

-- parte (a)
swapRow :: Int -> Int -> Matrix -> Matrix
swapRow x y m = swap x y m


swapColumn :: Int -> Int -> Matrix -> Matrix
swapColumn x y m = m >>= \xs -> [swap x y xs] 

swap :: Int -> Int -> [a] -> [a]
swap x y | x == y = id
         | otherwise = swap' (min x y) (max x y)

swap' :: Int -> Int -> [a] -> [a]
swap' first second lst = beginning ++ [y] ++ middle ++ [x] ++ end
  where
    (beginning, (x : r)) = splitAt first lst
    (middle, (y : end)) = splitAt (second - first - 1) r

-- parte (b)
swapM :: Size -> Matrix -> [Matrix]
swapM n m = do
  i <- [0..(n-1)]
  j <- [0..(n-1)]
  guard ((i/=j) && (i<j))
  (return (swapRow i j m)) ++ (return (swapColumn i j m))

-- parte (c)
swapMUntil :: Size -> (Matrix -> Bool) -> (Int, [Matrix]) -> (Int, [Matrix])
swapMUntil n p (i, ms) | (filter p ms) /= [] = (i, (filter p ms))
                       | otherwise = swapMUntil n p (i+1, (ms >>= \xs -> swapM n xs))

answer :: Int
answer = fst (swapMUntil 4 (== targetMatrix) (0,[sourceMatrix]))


{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}
type Disk = Int
data Peg = L | C | R deriving (Eq, Show)
type Conf = ([Disk], [Disk], [Disk])
type Move = (Peg,Peg)


-- parte (a)
push :: Disk -> Peg -> State Conf Conf
push d p = do
  (cl, cc, cr) <- get
  case p of
    L -> do
      put ([d] ++ cl ,cc,cr)
      return ([d] ++ cl,cc,cr)
    C -> do
      put (cl,[d] ++ cc ,cr)
      return (cl,[d] ++ cc,cr)
    R -> do
      put (cl,cc,[d] ++ cr)
      return (cl,cc,[d] ++ cr)

pop :: Peg -> State Conf Disk
pop p = do
  (cl, cc, cr) <- get
  case p of
    L -> do
      put (drop 1 cl,cc,cr)
      return (head cl)
    C -> do
      put (cl,drop 1 cc,cr)      
      return (head cc)
    R -> do
      put (cl,cc,drop 1 cr)      
      return (head cr)
  
-- parte (b)
step :: Move -> State Conf Conf
step (s, t) = do
  popped <- (pop s)
  pushed <- (push popped t)
  return pushed

-- complete la definicion


-- parte (c)
-- Retorna el tercer peg dado dos pegs
-- ej: compPeg (C,L) = R
compPeg :: (Peg, Peg) -> Peg
compPeg (p1,p2) = head (filter (\x -> (x /= p1) && (x /= p2)) [C,R,L])

optStrategy :: Int -> Move -> State Conf [(Move,Conf)]
optStrategy 1 m = do
    conf <- step m
    return [(m, conf)]
optStrategy n m@(s,t) = do
    conf1 <- optStrategy (n-1) (s, (compPeg m))
    conf2 <- optStrategy 1 m
    conf3 <- optStrategy (n-1) ((compPeg m), t)
    return (conf1 ++ conf2 ++ conf3)


-- parte (d)
{-
Las diferencias que se ven entre ambas implementaciones
es que la complejidad de las funciones pop/push es más
alta en comparación a su version normal dado que éstas 
funciones (pop y push) manejan directamente el estado
mientras que en la implementación anterior sólo modifican
un función.
Esto hace que optStrategy se hace más simple de implementar
siguiendo directamente la definición recursiva.
La solución implementada con mónadas de estado es una
solución más elegante para resolver este problema, dado
que la implementación de la funciones son más limpias y 
fáciles de trabajar una vez que se tiene un buen manejo 
de las mónadas de estado.

-}

-- parte (e)
makeInit :: Int -> Peg -> Conf 
makeInit n p = case p of
    L -> ([0..(n-1)],[],[])
    C -> ([],[0..(n-1)],[])
    R -> ([],[],[0..(n-1)])

play :: Int -> Peg -> Peg -> IO()
play n s t = putStr $ show initConf ++ foldr f v strat ++ "\n" where
  initConf  = makeInit n s
  v         = []
  f (m,c) r = "\n -> " ++ show m ++ " -> " ++ show c ++ r
  strat =  evalState (optStrategy n (s,t)) initConf
  
-- complete la definicion


--{-

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}
type Probability = Rational
type Dist a = T Probability a


-- Parte (a.I)
pointDist :: Int -> Dist (Int, Int)
pointDist r = do
  x <- uniform [-r..r]
  y <- uniform [-r..r]
  return (x,y)
-- complete la definicion

-- Parte (a.II)
resultE3a :: Int -> Probability
resultE3a r = 4 * ((\(x,y) -> x*x + y*y < r*r) ?? (pointDist r))

-- complete la definicion


-- Parte (b)
{-
Si le resulta conveniente, puede empezar siguiendo
el hint dado:
-}
data Uni     = Chile | Cato deriving (Eq, Show, Ord)
type Urn     = (Int, Int)
-- 1er componente: #jugadores Chile, 2do componente: #jugadores Cato
initUrn :: Urn
initUrn = (8,2)

pickPlayer :: Urn -> Dist Uni
pickPlayer (u,cato) = choose (toRational (u % (u+cato))) Chile Cato

pickSuccPlayer :: Int -> Uni -> Urn -> Probability
pickSuccPlayer 0 _ _ = 1
pickSuccPlayer n Chile urn@(u,cato) = ((\x -> x == Chile) ?? pickPlayer urn) * (pickSuccPlayer (n-1) Chile (u-1,cato))
pickSuccPlayer n Cato urn@(u,cato) = ((\x -> x == Cato) ?? pickPlayer urn) * (pickSuccPlayer (n-1) Chile (u,cato-1))

resultE3b :: Probability
resultE3b = let
  p1 = pickSuccPlayer 2 Cato initUrn 
  p2 = (pickSuccPlayer 2 Chile initUrn) * (pickSuccPlayer 2 Cato (6,2))
  p3 = (pickSuccPlayer 4 Chile initUrn) * (pickSuccPlayer 2 Cato (4,2))
  p4 = (pickSuccPlayer 6 Chile initUrn) * (pickSuccPlayer 2 Cato (2,2))
  p5 = (pickSuccPlayer 8 Chile initUrn) * (pickSuccPlayer 2 Cato (0,2))
  in
    1 - (p1 + p2 + p3 + p4 + p5)

---}
main :: IO()
main = return ()
