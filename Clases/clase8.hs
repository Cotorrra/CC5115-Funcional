{-
    Fold sobre listas

-}


length_me :: [a] -> Int
length_me xs = foldr (\_ y -> 1 + y) 0 xs



main :: IO ()
main = pure()