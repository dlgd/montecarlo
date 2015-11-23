module Pricing
    (
 -- * Pricing
      pricing

 -- * Payoff
    , callPayoff
    , putPayoff
    ) where

import Montecarlo
import System.Random
import Data.Random.Normal

callPayoff :: Double -> Double -> Double
callPayoff strike s = max (s - strike) 0

putPayoff :: Double -> Double -> Double
putPayoff strike s = max (strike - s) 0

normalMc :: Montecarlo StdGen Double
normalMc = liftMontecarlo normal

pricing :: Double -> Double -> Double ->
           Double -> (Double -> Double) ->
           Montecarlo StdGen (Average Double)
pricing s rate vol time_to_expiry payoff =
    f `fmap` normalMc
    where f z = average $ payoff $ s * exp (z * periodic_std_dv + periodic_mean)
          periodic_mean = (rate - 0.5 * vol ** 2) * time_to_expiry
          periodic_std_dv = vol * sqrt time_to_expiry
