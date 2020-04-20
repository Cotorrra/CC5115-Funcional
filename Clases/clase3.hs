
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, a) -> (a, a)
swap (x,y) = (y,x)

pair :: a -> a -> (a, a)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs


implies:: Bool -> Bool -> Bool
implies True False = False
implies _ _ = True

isMultipleOf3 :: Int -> Bool
isMultipleOf3 x | x >= 3 = isMultipleOf3 (x - 3) 
                | x >= 1 = False
                | x == 0 = True
                | otherwise = error "x debe ser positivo"

reverseMe :: [a] -> [a]
reverseMe [] = []
reverseMe (x : xs) = (reverseMe xs) ++ [x]

memberMe :: Eq a => a -> [a] -> Bool
memberMe _ [] = False
memberMe y (x:xs) = (y == x) || (memberMe y xs)

bmiTell :: Double -> Double -> String
bmiTell w h | bmi <= 18.5 = "underweight"
            | bmi <= 25.0 = "fine"
            | bmi <= 30.0 = "overweight"
            | otherwise = "health risk"
            where bmi = w / (h ** 2)

