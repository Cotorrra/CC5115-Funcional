import Control.Parallel.Strategies
import Data.List.Split
import Control.DeepSeq

type Number = Double

-- parte (a)
integral :: (Number -> Number) -> Number -> Number -> Int -> Number
integral f a b n = 
  let h = (a + b) / fromIntegral n
      g = \i -> f (a + fromIntegral i * h) + f (a + fromIntegral (i+1)*h)
  in h/2 * sum [g i | i <- [0..n]]


-- parte (b)
pintegral :: (Number -> Number) -> Number -> Number -> Int -> Number
pintegral f a b n =
  let h = (a + b) / fromIntegral n
      g = \i -> f (a + fromIntegral i * h) + f (a + (fromIntegral i+1)*h)
      sparks = chunksOf 50 [0..n]
      solutions =  
        map (sum . map g) sparks `using` parList rseq
        -- map (sum . map g) sparks `using` parList rpar
        -- parMap rseq (sum . map g) sparks
        -- parMap rpar (sum . map g) sparks
  in h/2 * sum solutions

-- parte (c)
-- Elapsed time sequential version: 0.025s
-- Elapsed time parallel version: 0.022s / 0.020s / 0.020s / 0.020s
-- Speedup: x1.13 / x1.25


main :: IO()
main = do
  let f = \x -> 2*x^2 + 3*x^10 - x^6 + 10*x^30 - 8*x^25
      a = 0
      b = 100
      n = 20000
  print (pintegral f a b n)
  return ()
