{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

-- Parte (a)
apps :: Double  -> Double -> [Double]
apps a r = [an] ++ (apps an r)
  where an = 0.5 * (a + r / a)

-- Parte (b)
approxLimit :: Double -> [Double] -> Double
approxLimit e (x:y:xs) | (x - y) < e = y 
                       | otherwise = approxLimit e (y:xs)
approxLimit _ xs = head xs -- Para evitar warnings

-- Parte (c)
approxSqrt :: Double -> Double -> Double -> Double
approxSqrt r a e = approxLimit e (apps a r)



{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

data Tree a = Node a [Tree a]

-- Parte (a)
itTree :: (a -> [a]) -> a -> Tree a
itTree fun r = Node r (map (itTree fun) (fun r))

-- Parte (b)
infTree :: Tree Integer
infTree = itTree (\x -> [(x+1)..]) 0

{-
-- Para probar la vaina
instance (Show a) => Show (Tree a) where
    show (Node a ts) = "\nRoot: ("++ (show a) ++ ")" ++ " Sons: {" ++ (show ts) ++ "}"

-- Créditos a: Vicente Reyes
finitize :: Int -> Int -> Tree a -> Tree a
finitize 0 _ (Node x _) = Node x []
finitize maxDepth maxLength (Node x ys)
  = Node x $ take maxLength $ map (finitize (maxDepth-1) maxLength) ys
-}

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}
{-
f :: [Int] -> (Int, Int) -> (Int, Int)
f []     c = c
f (x:xs) c = f xs (step x c)

step :: Int -> (Int,Int) -> (Int, Int)
step n (c0, c1) | even n    = (c0, c1 + 1)
                | otherwise = (c0 + 1, c1)
-}

-- Parte (a): call-by-value
{-
  f [1,2,3,4] (0,1)
= f [2,3,4] (step 1 (0,1))
= f [2,3,4] (0+1,1)
= f [2,3,4] (1, 1)
= f [3,4] (step 2 (1,1))
= f [3,4] (1,1+1)
= f [3,4] (1,2)
= f [4] (step 3 (1,2))
= f [4] (1 + 1,2)
= f [4] (2,2)
= f [] (step 4 (2,2))
= f [] (2,3)
= (2,2+3)
= (2,3)
-}

-- Parte (b): lazy evaluation
{-
  f [1,2,3,4] (0,1)
= f [2,3,4] (step 1 (0,1))
= f [3,4] (step 2 (step 1 (0,1)))
= f [4] (step 3 (step 2 (step 1 (0,1))))
= f [] (step 4 (step 3 (step 2 (step 1 (0,1)))))
= step 4 (step 3 (step 2 (step 1 (0,1))))
= step 4 (step 3 (step 2 ((0 + 1),1)))
= step 4 (step 3 ((0+1),(1+1)))
= step 4 ((0+1)+1,(1+1))
= ((0+1)+1,(1+1)+1)
= (1+1,(1+1)+1)
= (2,(1+1)+1)
= (2,2+1)
= (2,3)
-}

{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}
-- Parte (a)
{-
Hay un space leak en la reducción debido a la 
múltiple  cantidad de operaciones acumuladas 
de step debido a la evaluación lazy y otro 
asociado a las sumas de creadas por step.
-}

-- Parte (b)
{-
Para prevenir Space leaks, hay que forzar las evaluaciones
de step
-}
f :: [Int] -> (Int, Int) -> (Int, Int)
f []     c = c
f (x:xs) c = seq s (f xs s)
  where s = step x c

-- Además forzar las evaluaciones de suma.
step :: Int -> (Int,Int) -> (Int, Int)
step n (c0, c1) | even n    = seq c1' (c0, c1') 
                | otherwise = seq c0' (c0', c1)
    where c1' = c1 + 1 
          c0' = c0 + 1

-- Parte (c)

{-
length = length2 0 
length2 n [] = n 
length2 n (x : xs) = if n==0 then length2 1 xs 
                      else length2 (n+1) xs

length [1..10]
= length2 0 [1..10]
= lenght2 1 [2..10]
= lenght2 (1+1) [3..10]
= lenght2 ((1+1)+1) [4..10]
= lenght2 (((1+1)+1)+1) [5..10]
= ...
= lenght2 (..((1+1)...+1) []
= (..((1+1)...+1)
= ...
= 10

Hay un space leak con el acumulador de la suma
del largo, dado que se realizan las sumas sólo
al final de las llamadas a lenght2.
-}

-- Parte (d)
{-
Hay que forzar la suma en el acumulador.
length2 n [] = n 
length2 0 (x : xs) = length2 1 xs 
length2 n (x : xs) = seq next length2 next xs
  where next = n+1
-}



{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
{-
Definición original de partMerge
-}
partMerge :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
partMerge p (xs,ys) = (filter p xs ++ filter (not . p) ys,
                       filter (not . p) xs ++ filter p ys)
{-
Especificación de partcat
partcat p xs (us,vs) = (filter p xs ++ us, filter (not . p) xs ++ vs)
-}

-- Parte (a)
{-
Tanto las listas xs e ys se recorren 2 veces en 
computos distintos de forma innecesaria en partMerge.
-}

-- Parte (b)
{-
Notar la siguiente equivalencia de partMerge
partMerge p (xs,ys) = (filter p xs ++ notFilterY,
                       filter (not. p) xs ++ filterYs)
= partcat p xs (notFilterY, filterY)
Con notFilterYs = filter (not . p) ys  
     y filterYs = filter p ys 
Notar que esta tupla es equivalente a partcat p ys ([], [])
salvo que las posiciones están cambiadas.
partcat p ys ([], []) = (filterYs, notFilterYs)
Por lo que se pueden calcular utilizando pattern matching: 

partMerge :: (a->Bool) -> ([a],[a]) -> ([a],[a])
partMerge p (xs,ys) = partcat p xs (notFilterYs, filterYs)
  where (filterYs, notFilterYs) = partcat p ys ([],[])
-}

-- Parte (c)
partcat :: (a -> Bool) -> [a] -> ([a],[a]) -> ([a],[a])
partcat p xs (ys, zs) = foldl 
                            (\t x -> if (p x) 
                                    then ([x] ++ fst t, snd t) 
                                    else (fst t, [x] ++ snd t)) (ys, zs) (reverse xs)
-- Parte (d)
{-
Sí, hay una mejora significativa en espacio dado que 
se realizan las operaciones (++) primero, haciendo 
que reduzca el espacio utilizado en el cómputo de foldl'
-}

{-------------------------------------------}
{--------------  EJERCICIO 6  --------------}
{-------------------------------------------}
{-
-- Definición original
h :: Int -> Int
h 0 = 2
h 1 = 3
h 2 = 4
h n = 1 + 3 * h (n-1) + 3 * h (n-3)
----}

-- Parte (a)
{-
Las llamadas recursivas de h: h(n-1) y h(n-3) 
respectivamente provovcan una duplicación de cómputo,
causando un comportamiento exponencial en el tiempo 
de ejecución de la función.
-}

-- Parte (b)
h :: Int -> Int
h n = first (h2 n)

first :: (a,b,c) -> a
first (x, _, _) = x

h2 :: Int -> (Int, Int, Int)
-- h2 n = (h n, h (n-1), h (n-2))
h2 0 = (2,0,0)
h2 1 = (3,2,0)
h2 2 = (4,3,2)
h2 n = (1 + 3 * fn + 3 * fn2, fn, fn1) where (fn, fn1, fn2) = h2 (n-1)

main :: IO()
main = pure()
