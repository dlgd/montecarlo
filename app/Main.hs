module Main where

import Montecarlo
import Pricing
import System.Random

main :: IO ()
main =
    print $ getAverage $ evalMontecarloPar 100 10000 p (mkStdGen 42)
    where p = pricing 100 0.05 0.3 0.25 (putPayoff 100)

