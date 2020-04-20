{-- Funciones de alto orden:
    Las funciones en Haskell son miembros de primer orden, es decir,  
   pueden haber funciones que usen funciones en sus argumentos o tipos de retorno.
--}

-- Funciones de alto orden famosas

-- Soy el mapa
mapMe :: (a -> b)  -> [a] -> [b]
mapMe _ [] = []
mapMe f (x:xs) = f x : mapMe f xs

-- Soi el filter
filterMe :: (a -> Bool) -> [a] -> [a]
filterMe _ []     = []
filterMe p (x:xs) | p x = x : filterMe p xs
                  | otherwise = filterMe p xs

-- Aplico dos veces f
applyTwice :: (a -> a) -> (a -> a)
applyTwice f = g where g x = f (f x)
-- applyTwice f = \x -> f (f x)

-- Compongo dos funciones para hacer una tercera
compMe :: (b -> c) -> (a -> b) -> (a -> c)
compMe f g = h where h x = f (g x)
-- compMe f g = \x -> f (g x)

-- Crea una función constante con la variable que me dan
constMe :: a -> (b -> a)
constMe x = f where f _ = x -- Usamo la wildcard porque realmente no importa
-- constMe x = \_ -> x 

-- Funciones anonimas
-- (\x -> 2*x)

-- Funcion que crea una funcion que toma una listafkahsfliñhsuwdgliksrd
shortList :: Int -> ([a] -> Bool)
shortList n = \xs -> length xs < n
-- shortList n = f where f xs = length xs < n

-- Ejercicio:

-- Devuelve una funcion con los argumentos invertidos
flipMe :: (a -> b -> c) -> (b -> a -> c)
-- flipMe f = \x y -> f y x
flipMe f = g where g x y = f y x

selfComp :: Int -> (a -> a) -> (a -> a)
selfComp n f | n == 0 = id
             | n > 0 = f . (selfComp (n - 1) f)
             | otherwise = error "N must be positive"