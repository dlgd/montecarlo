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

import Control.Monad (replicateM)
import Control.Monad.State (State, evalState, state)
import Control.Parallel.Strategies (using, parList, rseq)
import qualified Data.Monoid as M (Monoid(..), mconcat)
import System.Random (RandomGen, next)

type Montecarlo g a = State g a

evalMontecarlo :: (RandomGen g, M.Monoid a) => Int -> Montecarlo g a -> g -> a
evalMontecarlo n mc g = M.mconcat $ evalState (replicateM n mc) g

evalMontecarloPar :: (RandomGen g, M.Monoid a) => Int -> Int -> Montecarlo g a ->
                     g -> a
evalMontecarloPar n c mc g = M.mconcat mcs
    where gs = take n (map snd (iterate (next . snd) (0, g)))
          mcs = map (evalMontecarlo c mc) gs `using` parList rseq

liftMontecarlo :: RandomGen g => (g -> (a, g)) -> Montecarlo g a
liftMontecarlo = state


data Average a = Average !Int !a

instance Num a => M.Monoid (Average a) where
    mempty = Average 0 0
    Average n a `mappend` Average m b = Average (n + m) (a + b)

average :: Fractional a => a -> Average a
average = Average 1

getAverage :: Fractional a => Average a -> a
getAverage (Average n a) = a / fromIntegral n


