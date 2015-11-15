module Main where

import Montecarlo

call :: Double -> Double -> Double
call strike s = max (s - strike) 0

samplesNum :: Int
samplesNum = 1000000

main :: IO ()
main =
  print $ montecarlo 100 0.05 0.3 0.25 (call 100) samplesNum
