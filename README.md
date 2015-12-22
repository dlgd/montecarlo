Montecarlo
====
[![Build Status](https://secure.travis-ci.org/dlgd/montecarlo.svg)](http://travis-ci.org/dlgd/montecarlo)

This toy package illustrates how to use montecarlo simulation for pricing simple options.

Purpose
----

This package provides a way to run a
[Monte Carlo](https://en.wikipedia.org/wiki/Monte_Carlo_method) simulation. A
Monte Carlo computation is represented by a `montecarlo` `monad` that outputs a
`monoid`. The computation can be evaluated multiple times and the result
aggregated into a final `monoid`.


Build
----

The preferred way to build this package is by using
[stack](http://www.haskellstack.org). Given `stack` is installed on your
machine, run:

    stack setup
    stack build
    stack test

It is also possible to use cabal by running:

    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests && cabal build && cabal test


Examples
----

A `Montecarlo` computation is created by calling the `montecarlo` function:

```haskell
ghci> :t montecarlo
:t montecarlo
montecarlo :: RandomGen g => (g -> (a, g)) -> Montecarlo g a
```

Evaluate 3 `Montecarlo` computations where each return a random `Double` wrapped
into a list. The final result is the concatenation of the 3 lists:

```haskell
ghci> evalMontecarlo 3 (pure `fmap` montecarlo random) (mkStdGen 42) :: [Double]
[1.0663729393723398e-2,0.9827538369038856,0.7042944187434987]
```

Create 100 random `Double` and sum them:

```haskell
ghci> evalMontecarlo 100 (Sum `fmap` montecarlo random) (mkStdGen 42) :: Sum Double
Sum {getSum = 48.04788200065967}
```

Create 10 * 1000 random `Double` and sum them. Here 10 chunks of 1000 are
evaluated in parallel.

```haskell
ghci> evalMontecarloPar 10 1000 (Sum `fmap` montecarlo random) (mkStdGen 42) :: Sum Int
Sum {getSum = -849043887812678664}
```

Create 10 random `Int` with a value between `1` and `10` and mulitply them:
```haskell
ghci> evalMontecarlo 10 (Product `fmap` montecarlo (randomR (1, 10))) (mkStdGen 42) :: Product Int
Product {getProduct = 176400000}
```


