module Main where

import Montecarlo
import System.Random
import Data.Monoid

call :: Double -> Double -> Double
call strike s = max (s - strike) 0

samplesNum :: Int
samplesNum = 1000000

randomMc :: Montecarlo StdGen Double
randomMc = liftMontecarlo random

mc :: Montecarlo StdGen (Sum Double)
mc = do
  x <- randomMc
  return (Sum x * 3)


data Average a = Average !Int !a deriving (Read, Show)

instance Num a => Monoid (Average a) where
    mempty = Average 0 0
    Average n a `mappend` Average m b = Average (n+m) (a+b)

runAverage :: Fractional a => Average a -> a
runAverage (Average n a) = a / fromIntegral n

mcAvg :: Montecarlo StdGen (Average Double)
mcAvg = do
  x <- randomMc
  return $ Average 1 (x * 3)

avg :: Fractional a => [a] -> a
avg xs = sum xs / realToFrac (length xs)

main :: IO ()
main = --do
--  print $ montecarlo 100 0.05 0.3 0.25 (call 100) samplesNum
  print $ runAverage $ evalMontecarlo 10 mcAvg (mkStdGen 42)
--  print $ getAverage $ evalMontecarloPar 1000 100 mcAvg (mkStdGen 42)

