module Montecarlo
    (
 -- * Montecarlo simulation
     Montecarlo
    , evalMontecarlo
    , evalMontecarloPar
    , liftMontecarlo

 -- * Monoid
    , Average
    , average
    , getAverage
    ) where

import Control.Monad.State
import Control.Parallel.Strategies
import Data.Monoid ()
import System.Random

type Montecarlo g a = State g a

evalMontecarlo :: (RandomGen g, Monoid a) => Int -> Montecarlo g a -> g -> a
evalMontecarlo n mc g = mconcat $ evalState (replicateM n mc) g

evalMontecarloPar :: (RandomGen g, Monoid a) => Int -> Int -> Montecarlo g a ->
                     g -> a
evalMontecarloPar n c mc g = mconcat mcs
    where gs = take n (map snd (iterate (next . snd) (0, g)))
          mcs = map (evalMontecarlo c mc) gs `using` parList rseq

liftMontecarlo :: RandomGen g => (g -> (a, g)) -> Montecarlo g a
liftMontecarlo = state


data Average a = Average !Int !a

instance Num a => Monoid (Average a) where
    mempty = Average 0 0
    Average n a `mappend` Average m b = Average (n + m) (a + b)

average :: Fractional a => a -> Average a
average = Average 1

getAverage :: Fractional a => Average a -> a
getAverage (Average n a) = a / fromIntegral n


