{-
    QuickCheck
-}

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck(modifyMaxSuccess)

-- Classic quicksort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
    smaller = [ a | a<-xs, a <= x]
    larger = [ a | a<-xs, a > x]

-- Traditional testing...
testQsort1 :: Spec
-- lo q ace la funcion
testQsort1 = describe "qsort function:" $ do
    -- Testing
    it "qsort [2,4,3,1] = [1,2,3,4]" $
        qsort ([2,4,3,1]::[Int]) `shouldBe` [1,2,3,4]
    it "qsort [] = []" $
        qsort ([]::[Int]) `shouldBe` []



isSorted:: Ord a => [a] -> Bool
isSorted [] = True
isSorted (_:[]) = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Testing basado en propiedades
-- Buscar propiedades más que pruebas especificas...
testQsort2 :: Spec
-- lo q ace la funcion
testQsort2 = describe "qsort function:" $
    modifyMaxSuccess (const 1500) $ do
    -- Testing
    it "qsort es idempotente" $
        property $ \xs-> qsort (qsort xs::[Int]) == qsort xs
    it "reversing does not affect sorting result" $
        property $ \xs -> qsort (reverse xs::[Int]) == qsort xs
    it "qsort orders the list" $
        property $ \xs -> isSorted (qsort xs::[Int])
    it "first memeber of the qsorted list is the min of the original" $
        property $ \xs -> not (null (xs::[Int])) ==> head (qsort xs) == minimum xs

{--
    The n-th Mersenne number is given M n = 2^n -1. 
    Since:
        M_2 =3, M_3 =7, M_5 =31 and M_7 =127
    it was conjectured for centuries that M n is prime whenever n is prime. Use
    QuickCheck infrastructure to prove that the conjecture is false. Recall that a
    natural number n is prime iff it has exactly two divisors in the range 1..n
    (namely 1 and n).
--}
factors:: Int -> [Int]
factors n = filter (\x -> (mod n x) == 0) [1..n]

isPrime:: Int -> Bool
isPrime n = [1,n] == (factors n)

testMersenne :: Spec
testMersenne = describe "Mersenne test:" $ 
    modifyMaxSuccess (const 1500) $ do
    it "Prime test" $ 
        property $ \x -> isPrime (x::Int) ==> isPrime ((2^x) - 1)


main :: IO ()
main = hspec $ do
    testMersenne