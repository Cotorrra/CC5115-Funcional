{-
    Funcion recursiva: Funcion que se autoinvoca


-}

-- Funcion rec clasica
-- Fibbonacci
fib :: Int -> Int
fib 0 = 0 -- caso base
fib 1 = 1 -- caso base
fib n | n > 0 = fib (n - 1) + fib (n - 2) -- recursion ...
      | otherwise = error "Negative argument" -- hay que ser precavidos con negs


{-
    Give a recursive definition of a function 
        expMe :: Num a => a -> Int -> a
        that computes a non-negative power of a number. For example, expMe
        0.5 3 should reduce to 0.125 . In case of receiving a negative exponent,
        it must throw an error.
-}

expMe :: Num a => a -> Int -> a
expMe _ 0 = 1
expMe n e | e > 0 = n * (expMe n (e - 1))
          | otherwise = error "Negative argument"

{-    
    Give a recursive function 
        merge :: Ord a => [a] -> [a] -> [a]
        that merges two sorted list to give a single sorted list. For example,
        merge [2,5,6] [1,3,4] should reduce to [1,2,3,4,5,6].
-}
merge :: Ord a => [a] -> [a] -> [a]
merge []    ys = ys
merge xs    [] = xs
merge (x:xs)  (y:ys) | x < y =  x : merge xs (y:ys)
                     | otherwise = y : merge (x:xs) ys

{-    
    Using merge, define a function
        msort :: Ord a => [a] -> [a]
        that implements the merge sort algorithm, in which the empty list and a
        singleton list are sorted, and any other list is sorted by merging together
        the two lists that result from sorting the two halves of the list separately
-}

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort h1) (msort h2)
    where (h1,h2) = splitAt (((length xs) + 1) `div` 2) xs


main:: IO () -- () es el unit, el tipo vacío
main = pure ()