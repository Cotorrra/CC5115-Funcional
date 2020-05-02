
slice :: [a] -> Int -> Int -> [a]
slice xs n m | (n > 0) && (m > 1)   = slice (tail xs) (n - 1) (m - 1)
             | (n == 0) && (m > 1)  = [head xs] ++ slice (tail xs) 0 (m-1)
             | (n == 0) || (m == 1) = []
             | (n < 0) || (n < 1)    = (error "Argumentos invalidos")
             | otherwise             = error "Cursed error"