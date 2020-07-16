import Control.Monad.State
-- import Numeric.Probability.Distribution hiding (map,coin,filter)



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
push = undefined
-- complete la definicion

pop :: Peg -> State Conf Disk
pop = undefined
-- complete la definicion


-- parte (b)
step :: Move -> State Conf Conf
step = undefined
-- complete la definicion


-- parte (c)
optStrategy :: Int -> Move -> State Conf [(Move,Conf)]
optStrategy = undefined
-- complete la definicion


-- parte (d)
{-
comple aqui su respuesta
-}


-- parte (e)
play :: Int -> Peg -> Peg -> IO()
play = undefined
-- complete la definicion


{-

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}
type Probability = Rational
type Dist a = T Probability a


-- Parte (a.I)
pointDist :: Int -> Dist (Int, Int)
pointDist = undefined
-- complete la definicion

-- Parte (a.II)
resultE3a :: Int -> Probability
resultE3a = undefined
-- complete la definicion


-- Parte (b)
{-
Si le resulta conveniente, puede empezar siguiendo
el hint dado:

data Uni     = Chile | Cato deriving Eq
type Urn     = (Int, Int)
-- 1er componente: #jugadores Chile, 2do componente: #jugadores Cato

pickPlayer :: Urn -> Dist (Uni, Urn)
pickPlayer = undefined
-}


resultE3b :: Probability
resultE3b = undefined
-- complete la definicion


-}

main :: IO()
main = return ()
