
-- P1
-- Escriba el tipo de las siguientes expresiones. Luego verifique utilizando ghci:
p1a :: [Char]
p1a = [ 'a', 'b', 'c' ]
p1b :: (Char, Char, Char)
p1b = ( 'a', 'b', 'c' )
p1c :: [(Bool,Int)]
p1c = [(False, 0), (True, 1)]
p1d :: ([Bool],[Int])
p1d = ([ False, True ], [ 0, 1 ])
p1e :: [[a] -> [a]]
p1e = [ tail, init, reverse ]

-- P2 
-- Escriba un programa con una definición para cada expresión. Luego, cargue su programa en
-- ghci y pruebe que tengan el tipo que usted esperaba.
bools :: [Bool]
bools = [True, False]
nums :: [[Int]]
nums = [[1,2,3],[2,4,5],[5,4,2]]
add :: Int -> Int -> Int -> Int
add x y z = x + y + z 
copy :: a -> (a, a)
copy x = (x, x)
apply :: (a -> b) -> a -> b
apply (x) = x
{-- Funciones
    P3 
Usando funciones conocidas, escriba la función halve :: [a] -> ([a], [a]) que toma una lista de
cantidad de elementos pares y retorna una tupla con las dos mitades de la lista. Debe escribir esta
función en el archivo aux1p3.hs. Finalmente, cargue su archivo en ghci y pruebe que funciona
correctamente. Hint: Puede usar la función splitAt :: Int -> [a] -> ([a], [a]) que corta la lista en el
indice indicado.
Código 1: halve
halve [ 1, 2, 3, 4, 5, 6 ] == ([ 1, 2, 3 ], [ 4, 5, 6 ])
--}
halve :: [a] -> ( [a] , [a] )
halve xs = splitAt ((length xs) `div` 2) xs
{--
P4 (aux1p4.hs)
Escriba la función third :: [a] -> a que retorna el tercer elemento de una lista que contiene al
menos 3 elementos. Escriba dos versiones de esta función: utilizando head y tail. Puede llamarla
thirdHT; utilizando pattern matching. Puede llamarla thirdPM. Escriba sus funciones en el archivo
aux1p4.hs y luego carguelo en ghci para testear.
--}
thirdHT :: [a] -> a
thirdHT xs = head (tail (tail xs))

-- thirdPM :: [a] -> a
-- thirdPM (_:_:x:_) = x

{--
P5 (aux1p5.hs)
Considere la función safetail que se comporta igual que la función tail pero que en el caso de
la lista vacía no arroja un error si no que devuelve dicha lista. Utilizando tail y null :: [a] -> Bool,
que retorna True si una lista esta vacía, defina safetail usando:
Conditional expressions. (safetailCE)
Guarded equations. (safetailGE)
Pattern matching. (safetailPM)
--}

safetailCE :: [a] -> [a]
safetailCE xs = if null xs then xs else tail xs

safetailPM :: [a] -> [a]
safetailPM [] = []
safetailPM xs = tail xs

safetailGE :: [a] -> [a]
safetailGE xs | null xs = []
              | otherwise = tail xs

{--
P6 (aux1p6.hs)
El algoritmo de Luhn es utilizado para validar números de tarjetas de crédito, y funciona de la
siguiente manera:
    - Se toma cada dígito de la tarjeta como un número individual.
    - Se ignora el primer número.
    - Se dobla cada número.
    - Se le resta 9 a todos los números mayores que 9.
    - Se suman todos los resultados.
Si el total es divisible por 10 entoncesa la tarjeta es válida.
Defina una función luhnDouble :: Int -> Int que dobla un dígito y le resta 9 si es que el resultado
es mayor que 9. Luego defina luhn :: Int -> Int -> Int -> Int -> Bool que verifica si una tarjeta de
4 dígitos es válida. Por ejemplo:
Código 2: halve
- > luhnDouble 3 == 6
- > luhnDouble 6 == 3
- > luhn 1 4 7 8 == True
- > luhn 4 7 8 3 == False
--}

luhnDouble :: Int -> Int
luhnDouble x | x < 5 = x * 2
             | otherwise = (x * 2) - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn u v x y = (mod suma 10) == 0 where suma = u + luhnDouble v + luhnDouble x + luhnDouble y


