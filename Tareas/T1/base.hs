import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck(modifyMaxSuccess)

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
addTuple (a,b) (c,d) = (a + c, b + d)


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
          where result = splitAtFirstHit2 p (x:xs) []


splitAtFirstHit2 :: (a -> Bool) -> [a] -> [a] -> [[a]]
splitAtFirstHit2 _ [] ys = [ys]
splitAtFirstHit2 p (x:xs) ys | p x = [ys] ++ (splitAtFirstHit2 (\_ -> False) xs [x])
                             | otherwise = splitAtFirstHit2 p xs (ys ++ [x])


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
isMember x xs = hasSomeHit (x==) xs


repeteadElem :: Eq a => [a] -> Bool
repeteadElem [] = False
repeteadElem (x:xs) = (hasSomeHit (==x) xs) || repeteadElem xs
-- repeteadElem (x:xs) = (isMember x xs) || repeteadElem xs


applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
applyUntil f p b | p b = b
                 | otherwise = applyUntil f p (f b)


leastPow2 :: Int -> Int
leastPow2 n | n < 0 = error "argumento negativo"
            | otherwise = applyUntil (\x -> effPow x 2) (\y -> y > n) 2


balancedSufix :: [Bool] -> [Bool]
balancedSufix [] = []
balancedSufix (x:xs) = applyUntil f p (x:xs)
    where f = (\ys -> tail ys) -- Saco un sufijo de xs
          p = (\ys -> ((numberOfHits (id) ys) == (numberOfHits (\y -> not y) ys)))


{-------------------------------------------}
{--------------  EJERCICIO 4  --------------}
{-------------------------------------------}

{-
Provea derivacion de isMemberPF
-}

-- isMemberPF ::
isMemberPF :: Eq a => a -> [a] -> Bool
-- isMemberPF x xs = hasSomeHit (== x) xs
-- isMemberPF x = hasSomeHit (== x)
isMemberPF x = hasSomeHit . (==) $ x   
-- isMemberPF x = (hasSomeHit . (==)) $ x
-- isMemberPF x = (hasSomeHit . (==)) x
-- isMemberPF = (hasSomeHit . (==)) 
-- isMemberPF = hasSomeHit . (==)


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
testNumberOfHits = describe "testNumbehrOfHits function:" $ do
    it "++ por sobre +" $
      property $ \xs ys -> numberOfHits even ((xs::[Int]) ++ (ys::[Int])) == numberOfHits even xs + numberOfHits even ys
    it "largo de xs" $
      property $ \xs -> numberOfHits (\_ -> True) (xs::[Int]) == length xs
    it "null" $
      property $ \xs -> numberOfHits (\_ -> False) (xs::[Int])  == 0


trueP :: a -> Bool
trueP _ = True

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] _ = []
intersection (x:xs) ys | isMember x ys = [x] ++ intersection xs ys
                       | otherwise = intersection xs ys

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys | isMember x ys = union xs ys
                | otherwise = [x] ++ union xs ys

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) | isMember x xs = xs
              | otherwise = [x] ++ unique xs

testExtraNumberOfHits :: Spec
testExtraNumberOfHits = describe "testExtraNumberOfHits function:" $ do
    it "Inclusion-Exclusion" $
      property $ \xs ys -> numberOfHits trueP (union (unique (xs::[Int])) (unique (ys::[Int]))) == 
        (numberOfHits trueP (unique xs)) + (numberOfHits trueP (unique ys)) - numberOfHits trueP (intersection (unique xs) (unique ys))


main:: IO()
main = hspec $ do
  testEffPow
  testPay
  testNumberOfHits
  testExtraNumberOfHits

