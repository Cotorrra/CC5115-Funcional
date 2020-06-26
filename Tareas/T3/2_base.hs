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
itTree f r = Node r (map (itTree f) (f r))


-- Parte (b)
infTree :: Tree Integer
infTree = itTree (\x -> [(x+1)..]) 0



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

= f [2,3,4] (step 1 (0,1))
= f [2,3,4] (1, 1)
= f [3,4] (step 2 (1,1))
= f [3,4] (1,2)
= f [4] (step 3 (1,2))
= f [4] (2,2)
= f [] (step 4 (2,2))
= f [] (2,3)
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
= step 4 (step 3 (step 2 (0 + 1,1)))
= step 4 (step 3 (step 2 (1,1)))
= step 4 (step 3 (1,1 + 1))
= step 4 (step 3 (1,2))
= step 4 (1 + 1,2)
= step 4 (2,2)
= (2,2 + 1)
= (2,3)
-}

{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}
-- Parte (a)
{-
Hay un space leak en la reducción debido a la múltiple 
cantidad de operaciones acumuladas de step debido a la
evaluación lazy.
-}

-- Parte (b)
{-
Hay que forzar las evaluaciones de step para prevenir 
los space leaks
f :: [Int] -> (Int, Int) -> (Int, Int)
f []     c = c
f (x:xs) c = seq s (f xs s)
  where s = step x c

-}

-- Parte (c)
{-

-}

-- Parte (d)
{-
En caso que corresponda, agregue aquí su nueva
definición de length y/o length2
-}



{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}
{-
Definición original de partMerge
partMerge :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
partMerge p (xs,ys) = (filter p xs ++ filter (not . p) ys,
                       filter (not . p) xs ++ filter p ys)

Especificación de  partcat
partcat p xs (us,vs) = (filter p xs ++ us, filter (not . p) xs ++ vs)
-}

-- Parte (a)
{-
Agregue aquí su respuesta
-}

-- Parte (b)
{-
Agregue aquí su derivación formal de partMerge
-}

-- Parte (c)
-- partcat :: (a -> Bool) -> [a] -> ([a],[a]) -> ([a],[a])
-- Descomente el tipo y agregue su definición


-- Parte (d)
{-
Agregue aquí su respuesta
-}



{-------------------------------------------}
{--------------  EJERCICIO 6  --------------}
{-------------------------------------------}
{-
Definición original
h :: Int -> Int
h 0 = 2
h 1 = 3
h 2 = 4
h n = 1 + 3 * h (n-1) + 3 * h(n-3)
-}

-- Parte (a)
{-
Agregue aquí su respuesta
-}

-- Parte (b)
-- h :: Int -> Int
-- Descomente el tipo y agregue su definición



main :: IO()
main = pure()
