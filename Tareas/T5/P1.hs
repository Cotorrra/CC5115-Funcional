import Data.Array
import Control.Parallel.Strategies
import Control.Exception
import Data.List.Split

type Coeff = Integer
type Poly = Array Int Coeff

mkPoly :: [Coeff] -> Poly
mkPoly xs = listArray (0,d) xs where d = length xs - 1

degree :: Poly -> Int
degree = snd . bounds

coeff :: Poly -> Int -> Coeff
coeff p i | i <= degree p = p ! i
          | otherwise     = 0


-- parte (a)
coeffProd :: Poly -> Poly -> Int -> Coeff
coeffProd p1 p2 d = sum [ c1*c2 | (d1,c1) <- assocs p1, (d2,c2) <- assocs p2, d1+d2 == d]


seqProd :: Poly -> Poly -> Poly
seqProd p1 p2 = mkPoly (map (coeffProd p1 p2) [0..d]) 
  where
    d = degree p1 + degree p2


-- parte (b)
-- (Comando hecho con -O2)
-- Elapsed time: 30.380s elapsed


-- parte (c)
parProd :: Poly -> Poly -> Poly
parProd p1 p2 = mkPoly (concat para)
  where
    d = degree p1 + degree p2
    func = map (coeffProd p1 p2)
    sparks = chunksOf 15 [0..d]
    para = 
      map func sparks `using` parList rseq
      -- map func sparks `using` parList rpar
      -- parMap rseq func sparks
      -- parMap rpar func sparks
    
{- parte (d)
-- Elapsed time: 26.704s 
--               26.549s
--               24.839s
--               24.835s
-- parte (e)
-- Speedup: 1.13
--          1.14
--          1.22
--          1.22
-}

-- parte (f)
par1Prod :: Poly -> Poly -> Poly
par1Prod p1 p2 = mkPoly sol
  where
    d = degree p1 + degree p2
    sol = 
      map (coeffProd p1 p2) [0..d] `using` parList rseq
      -- map (coeffProd p1 p2) [0..d] `using` parList rpar
      -- parMap rseq (coeffProd p1 p2) [0..d]
      -- parMap rpar (coeffProd p1 p2) [0..d]
      

-- parte (g) 30.380s 
-- Elapsed time: 25.241s
--               17.052s
--               21.536s
--               18.273s
-- Speedup: 1.17 
--          1.78
--          1.41
--          1.66

-- parte (h)
{-
Dado que se utilza paralelización vista en clases 
(la que no está comentada) ésta resulta ineficiente 
dado que el cómputo de coeffProd se realiza de manera 
secuencial una vez terminado el cómputo paralelo de los 
elementos de la lista.
Realizar éstos cómputos tambien en paralelo en conjunto al map
de los elementos trae mejores tiempos de cómputo.
[Esto se ve cuando se realizan los otros cómputos de las
otras estrategias aplicadas.]
-}

-- parte (i)
{-
Debido a que utilizar un print de (seqProd pa pb) 
solo va a calcular el producto a medida que el print
lo vaya necesitando, debido a Haskell es un lenguaje Lazy.
Por lo que éste se debe evaluar anteriormente utilizando 
nonNullCoeff. 
-}

-- Determina el número de coeficientes no nulos de un polinomio
nonNullCoeff :: Poly -> Int
nonNullCoeff = foldr (\c rec -> if c == 0 
  then rec 
  else rec + 1) 0

main :: IO()
main = do
  let pa = mkPoly [100..2000]
      pb = mkPoly [2000..5000]
  print (nonNullCoeff (par1Prod pa pb))
  return ()
