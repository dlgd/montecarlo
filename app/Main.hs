module Main where

import Montecarlo
import System.Random
import Data.Monoid.Average

call :: Double -> Double -> Double
call strike s = max (s - strike) 0

samplesNum :: Int
samplesNum = 1000000

randomMc :: Montecarlo StdGen Double
randomMc = liftMontecarlo random

mc :: Montecarlo StdGen Double
mc = do
  x <- randomMc
  return $ x * 3

avg :: Fractional a => [a] -> a
avg xs = sum xs / realToFrac (length xs)

main :: IO ()
main = do
--  print $ montecarlo 100 0.05 0.3 0.25 (call 100) samplesNum
  print $ evalMontecarlo 10 mc avg (mkStdGen 42)
  print $ evalMontecarloPar 1000 100 mc avg (mkStdGen 42)

