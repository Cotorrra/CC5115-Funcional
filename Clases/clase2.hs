{--
    Funciones en Haskell
--}

-- Las funciones hacen de todo :o
double :: Num a => a -> a
double x = x + x

-- Pueden hacer llamados a otras funciones
quadruple :: Num a => a -> a
quadruple x = double (double x)

-- Funciones sobre listas
-- head :: [a] -> a
-- Retorna la cabeza de la lista
-- ex: head [1,2,3,4,5] = 1
initMe :: Num a => [a] -> [a]
initMe [x, _] = [x]
initMe xs = head xs : initMe (tail xs)


-- tail :: [a] -> [a]
-- Retorna la cola de la lista
-- ex: tail [1,2,3,4,5] = [2,3,4,5]


lastMe :: [a] -> a 
lastMe [_, y] = y
lastMe xs = lastMe (tail xs)

-- take :: Int -> [a] -> [a]
-- Retorna una lista con los primeros n numeros de ésta.
-- ex: take 3 [1,2,3,4,5] = [1,2,3]
partialSum :: Int -> [Double] -> Double
partialSum n xs = sum (take n xs)

-- drop :: Int -> [a] -> [a]
-- Retorna una lista con los primeros n numeros removidos de ésta.
-- ex: take 3 [1,2,3,4,5] = [4,5]
excludeSum :: Int -> [Double] -> Double
excludeSum n xs = sum (drop n xs)
