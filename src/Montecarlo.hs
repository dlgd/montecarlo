module Montecarlo
    (
     montecarlo
    ) where

import Data.Random.Normal
import Control.Parallel.Strategies

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
      payoffs = map (montecarlo' s rate vol time_to_expiry payoff chunkSize) seeds
                `using` parList rdeepseq
  sum payoffs / realToFrac chunksCount

