{--
    Currificación: Curry y la CTM

Haskell solo permite tener funciones de una variable
Para emular multiples argumentos se utiliza la currificación

--}


-- Empaquetar argumentos en tuplas.
-- Funciones no currificadas...
addNC :: (Int, Int) -> Int
addNC (x,y) = x + y

maxThreeNC :: (Int, Int, Int) -> Int
maxThreeNC (x,y,z) = max x (max y z)

-- Currificado: Funcion dentro de una funcion...
addC :: Int -> (Int -> Int)
addC = \x -> (\y -> x + y)
-- addC x = \y -> x + y

maxThreeC :: Int -> (Int -> (Int -> Int))
maxThreeC = \x -> (\y -> (\z -> (max x (max y z))))
-- Esta super claro que la definicion de la funcion se parece al tipo.

{-
    Haskell tiene diferentes convenciones para las cosas de arriba:
    - El operador -> asocia a la derecha así que los tipos anteriores quedan:
        Int -> (Int -> (Int -> Int))  == Int -> Int -> Int -> Int
    - Tambien tiene un azucar sintáctico para las funciones de multiples argumentos:
        add x y z = ???  <==> add = \x -> \y -> \z -> ???
    - La aplicacion de funciones asocia hacia la izquierda:
        addc 2 3 = (addc 2) 3
        maxThreeC 1 2 3 = ((maxThreeC 1) 2) 3
-}

-- Ejercicios:

{-  
    Define and provide the type of a function curryMe that takes an
    uncurried function of two arguments and returns it curried version.
-}

curryMe :: ((a, b) -> c) -> a -> b -> c
curryMe f = \u v -> f (u,v)

{-
    Provide a definition for the parallel composition operator
    parComp :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
-}

parComp :: (a -> b) -> (c -> d) -> ((a,c) -> (b,d))
parComp f g = \(x,y) -> (f x, g y)


{-
    Provide the definition of a function 
    shorten :: [String] -> [String]
    that given a list of strings keeps only the first 5 characters of each
    string. (If the string has less than 5 characters, just keep it as is.)
-}

shorten :: [String] -> [String]
shorten xs = map (take 5) xs -- wow 

-- Pointfree vs Pointwise

-- Pointwise: Escribimo el argumento de la funcion
applyTwicePW :: (a -> a) -> (a -> a)
applyTwicePW f x = (f . f) x

-- Pointfree: NO Escribimo el argumento de la funcion
applyTwicePF :: (a -> a) -> (a -> a)
applyTwicePF f = f . f

{-
    Give a pointfree definition of function
    howManyAs :: String -> Int
    that counts the numbers of (lowercase) a’s of a string. 
    Hint: do not make the function recursive. Instead, define it using
    length and filter 
-}
howManyAs :: String -> Int
howManyAs = length . filter ('a'==)

main:: IO () -- () es el unit, el tipo vacío
main = pure ()