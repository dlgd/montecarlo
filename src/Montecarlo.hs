module Montecarlo
    (
      montecarlo
    , Montecarlo
    , evalMontecarlo
    , evalMontecarloPar
    , liftMontecarlo
    ) where

import Data.Random.Normal
import Control.Parallel.Strategies
import System.Random
import Control.Monad.State
import Control.Monad
import Data.Monoid

montecarlo' :: Double -> Double -> Double ->
               Double -> (Double -> Double) -> Int -> Int -> Double
montecarlo' s rate vol time_to_expiry payoff n seed =
  sum payoffs / realToFrac n
  where payoffs = map f (take n (mkNormals seed))
        periodic_mean = (rate - 0.5 * vol ** 2) * time_to_expiry
        periodic_std_dv = vol * sqrt time_to_expiry
        f z = payoff s_cur
            where s_cur = s * exp (z * periodic_std_dv + periodic_mean)


montecarlo :: Double -> Double -> Double ->
              Double -> (Double -> Double) -> Int -> Double
montecarlo s rate vol time_to_expiry payoff n = do
  let chunkSize = 1000
      chunksCount = n `div` chunkSize
      seeds = take chunksCount [1..]
      payoffs = map (montecarlo' s rate vol time_to_expiry payoff chunkSize)
                seeds `using` parList rdeepseq
  sum payoffs / realToFrac chunksCount

{--
toto :: Double
toto = fst $ normal (mkStdGen 42)

foo :: State StdGen Double
foo = do
  g <- get
  state $ random
--}

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

