-- Introducción a Haskell:
{-- 
    Es un lenguaje de programación funcional, es decir se utlizan aplicaciones 
    de funciones a estructuras de datos inmutables.
--}

-- Los programas de Haskell son más elegantes y concisos.
-- Por ejemplo:


-- Haskell es estáticamente tipado, cada parte del código tiene su tipo determinado.
mySum :: Int -> Int
mySum 0 = 0
mySum n = n + (mySum (n - 1))

-- Haskell puede adivinar los tipos de las funciones
-- Quicksort en Haskell
qsort :: Ord a => [a] -> [a] -- Contrato de QuickSort
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where 
        smaller = [a | a <- xs, a <= x] -- Adivina que es de tipo [a]
        larger  = [b | b <- xs, b > x]  -- Adivina que es de tipo [a]

-- La programación funcional nos permite ser más matemáticos y aplicar funciones reales
-- Esto nos permite saber exactamente lo qué pasa con los elementos que usamos.

-- Haskell es Lazy, es decir sólo evalúa las cosas cuando realmente tiene que hacerlo.
-- Por lo que hay estas joyas como listas infinitas.
pow2 :: [Int]
pow2 = 1 : map (2*) pow2
