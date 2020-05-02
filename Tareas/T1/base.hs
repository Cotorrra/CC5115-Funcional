import Test.Hspec
import Test.QuickCheck


{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

effPow :: Num a => a -> Int -> a
effPow b 1 = b
effPow b n | n < 0 = error "exponente negativo" 
           | even n = (effPow b m) * (effPow b m) 
           | otherwise = (effPow b m) * (effPow b m) * b where
            m = case even n of
              True -> n `quot` 2
              False -> (n - 1) `quot` 2

pay :: Int -> (Int, Int)
pay 8 = (1,1)
pay 9 = (3,0)
pay 10 = (0,2)
pay n | n < 8 = error "no se pueden pagar " n " pesos"
      | otherwise = addTuple (1,0) (pay (n - 3))


addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (a,b) (c,d) = (a+c, b+d)


{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

numberOfHits :: (a -> Bool) -> [a] -> Int
numberOfHits _ [] = 0
numberOfHits p (x:xs) | p x = 1 + numberOfHits p xs
                      | otherwise = 0 + numberOfHits p xs


splitAtFirstHit :: (a -> Bool) -> [a] -> ([a],[a])
splitAtFirstHit _ [] = error "no hit in the list"
splitAtFirstHit p (x:xs) 
          | length result == 2 = (head result, head (tail result))
          | length result == 1 = error "no hit in the list"
          | otherwise = error "cursed error"
          where result = splitAtFirstHit2 p (x:xs)


splitAtFirstHit2 :: (a -> Bool) -> [a] -> [[a]]
splitAtFirstHit2 _ [] = []
splitAtFirstHit2 p (x:xs) | p x = splitAtFirstHit2 (\_ -> False) xs
                          | otherwise = [x] ++ (splitAtFirstHit2 (\_ -> False) xs)


positionsAllHits :: (a -> Bool) -> [a] -> [Int]
positionsAllHits _ [] = []
positionsAllHits p (x:xs) = countPositions p (x:xs) 0

countPositions :: (a -> Bool) -> [a] -> Int -> [Int]
countPositions _ [] _ = [] 
countPositions p (x:xs) n | p x = [n] ++ (countPositions p xs (n+1)) 
                          | otherwise = (countPositions p xs (n+1)) 


evens :: [a] -> [a]
evens [] = []
evens (x:xs) = [x] ++ (odds xs)

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

hasSomeHit :: (a -> Bool) -> [a] -> Bool
hasSomeHit _ [] = False
hasSomeHit p (x:xs) | p x = True
                    | otherwise = hasSomeHit p xs

isMember :: Eq a => a -> [a] -> Bool
isMember x xs = hasSomeHit (\y -> x == y) xs


repeteadElem :: Eq a => [a] -> Bool
repeteadElem [] = False
repeteadElem (x:xs) = (isMember x xs) && repeteadElem xs


applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
applyUntil f p b | p b = b
                 | otherwise = applyUntil f p (f b)

leastPow2 :: Int -> Int
leastPow2 n | n < 0 = error "argumento negativo"
            | otherwise = applyUntil (\x -> effPow x 2) (\y -> y > n) 2


-- balancedSufix :: [Bool] -> [Bool]
-- balancedSufix xs | 

-- complete tipo y definicion, y descomente la linea de arriba




{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

{-
Provea derivacion de isMemberPF
-}

-- isMemberPF ::
-- isMemberPF :: Eq a => a -> [a] -> Bool
-- isMemberPF x xs = hasSomeHit (\y -> x == y) xs
-- isMemberPF x xs = (\y -> x == y) `hasSomeHit`  xs
-- isMemberPF x xs = (== x) `hasSomeHit`  xs
-- isMemberPF x xs = (`hasSomeHit` xs) (==x)
-- isMemberPF x xs = ((`hasSomeHit` xs) ==) x



{-------------------------------------------}
{--------------  EJERCICIO 5  --------------}
{-------------------------------------------}

testEffPow :: Spec
testEffPow = describe "testEffPow function:" $ do
    it "La potencia de la multiplicacion es la multiplicacion de la potencia" $
        property $ \x y n -> (n::Int) > 0 ==> effPow ((x::Int) * (y::Int)) n == (effPow x n) * (effPow y n)
    it "La suma de exponentes es la multiplicacion de potencias" $
        property $ \x m n -> ((n::Int) > 0) ==> ((m::Int) > 0) ==> effPow (x::Int) (n + m) == (effPow x m) * (effPow x n)
    

testPay :: Spec
testPay = describe "testPay function:" $ do
    it "Pay funciona como deberia" $
        property $ \n -> (n::Int) >= 8 ==> (3 * (fst (pay n))) + (5 * (snd (pay n))) == n
    
testNumberOfHits :: Spec
testNumberOfHits = describe "testNumberOfHits function:" $ do
    it "" $
      property $ \xs ys -> numberOfHits even ((xs::[Int]) ++ (ys::[Int])) == numberOfHits even xs + numberOfHits even ys
    it "" $
      property $ \xs -> numberOfHits (\_ -> True) (xs::[Int]) == length xs
    it "" $
      property $ \xs -> numberOfHits (\_ -> False) (xs::[Int])  == 0

{-
testExtraNumberOfHits :: Spec
testExtraNumberOfHits = describe "testNumberOfHits function:" $ do
    it "" $
      property $ 
-}

main:: IO()
main = hspec $ do
  testEffPow
  testPay
  testNumberOfHits
  -- testExtraNumberOfHits

