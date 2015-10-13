# declarative

[![Build Status](https://secure.travis-ci.org/jtobin/declarative.png)](http://travis-ci.org/jtobin/declarative)
[![Hackage Version](https://img.shields.io/hackage/v/declarative.svg)](http://hackage.haskell.org/package/declarative)

DIY Markov Chains.

## What Is This

This package presents a simple combinator language for Markov transition
operators that are useful in MCMC.

Any transition operators sharing the same stationary distribution and obeying
the Markov and reversibility properties can be combined in a couple of ways,
such that the resulting operator preserves the stationary distribution and
desirable properties amenable for MCMC.

We can deterministically concatenate operators end-to-end, or sample from
a collection of them according to some probability distribution.  See
[Geyer, 2005](http://www.stat.umn.edu/geyer/f05/8931/n1998.pdf) for details.

The result is a simple grammar for building composite, property-preserving
transition operators from existing ones:

    transition ::= primitive <transition>
                 | concatT transition transition
                 | sampleT transition transition

This library also re-exports a number of production-quality transition
operators from the
[mighty-metropolis](hackage.haskell.org/package/mighty-metropolis),
[speedy-slice](hackage.haskell.org/package/speedy-slice), and
[hasty-hamiltonian](hackage.haskell.org/package/hasty-hamiltonian) libraries.

Markov chains can then be run over arbitrary `Target`s using whatever
transition operator is desired.

    import Numeric.MCMC
    import Data.Sampling.Types

    target :: [Double] -> Double
    target [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

    rosenbrock :: Target [Double]
    rosenbrock = Target target Nothing

    transition :: Transition IO (Chain [Double] b)
    transition =
      concatT
        (sampleT (metropolis 0.5) (metropolis 1.0))
        (sampleT (slice 2.0) (slice 3.0))

    main :: IO ()
    main = withSystemRandom . asGenIO $ mcmc 10000 [0, 0] transition rosenbrock

![trace](https://dl.dropboxusercontent.com/spa/u0s6617yxinm2ca/b2w56upc.png)

## Installation

Installing is best done via
[stack](https://github.com/commercialhaskell/stack), which will pull down
everything you might need (including GHC).

You'll want to use the [Stackage nightly
resolver](https://www.stackage.org/nightly) until the next LTS version picks
up these libraries.

With that out of the way it's just a matter of

```
$ stack install declarative
```
