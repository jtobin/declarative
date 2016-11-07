{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Numeric.MCMC
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- This module presents a simple combinator language for Markov transition
-- operators that are useful in MCMC.
--
-- Any transition operators sharing the same stationary distribution and
-- obeying the Markov and reversibility properties can be combined in a couple
-- of ways, such that the resulting operator preserves the stationary
-- distribution and desirable properties amenable for MCMC.
--
-- We can deterministically concatenate operators end-to-end, or sample from
-- a collection of them according to some probability distribution.  See
-- <www.stat.umn.edu/geyer/f05/8931/n1998.pdf Geyer, 2005> for details.
--
-- The result is a simple grammar for building composite, property-preserving
-- transition operators from existing ones:
--
-- @
-- transition ::= primitive <transition>
--              | concatT transition transition
--              | sampleT transition transition
-- @
--
-- In addition to the above, this module provides a number of combinators for
-- building composite transition operators.  It re-exports a number of
-- production-quality transition operators from the /mighty-metropolis/,
-- /speedy-slice/, and /hasty-hamiltonian/ libraries.
--
-- Markov chains can then be run over arbitrary 'Target's using whatever
-- transition operator is desired.
--
-- > import Numeric.MCMC
-- > import Data.Sampling.Types
-- >
-- > target :: [Double] -> Double
-- > target [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
-- >
-- > rosenbrock :: Target [Double]
-- > rosenbrock = Target target Nothing
-- >
-- > transition :: Transition IO (Chain [Double] b)
-- > transition =
-- >   concatT
-- >     (sampleT (metropolis 0.5) (metropolis 1.0))
-- >     (sampleT (slice 2.0) (slice 3.0))
-- >
-- > main :: IO ()
-- > main = withSystemRandom . asGenIO $ mcmc 10000 [0, 0] transition rosenbrock
--
-- See the attached test suite for other examples.

module Numeric.MCMC (
    concatT
  , concatAllT
  , sampleT
  , sampleAllT
  , bernoulliT
  , frequency
  , anneal
  , mcmc

  -- * Re-exported
  , module Data.Sampling.Types

  , metropolis
  , hamiltonian
  , slice

  , MWC.create
  , MWC.createSystemRandom
  , MWC.withSystemRandom
  , MWC.asGenIO

  , PrimMonad
  , PrimState
  , RealWorld
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Sampling.Types
import Numeric.MCMC.Anneal
import Numeric.MCMC.Metropolis hiding (mcmc)
import Numeric.MCMC.Hamiltonian hiding (mcmc)
import Numeric.MCMC.Slice hiding (mcmc)
import Pipes hiding (next)
import qualified Pipes.Prelude as Pipes
import System.Random.MWC.Probability (Gen)
import qualified System.Random.MWC.Probability as MWC

-- | Deterministically concat transition operators together.
concatT :: Monad m => Transition m a -> Transition m a -> Transition m a
concatT = (>>)

-- | Deterministically concat a list of transition operators together.
concatAllT :: Monad m => [Transition m a] -> Transition m a
concatAllT = foldl1 (>>)

-- | Probabilistically concat transition operators together.
sampleT :: PrimMonad m => Transition m a -> Transition m a -> Transition m a
sampleT = bernoulliT 0.5

-- | Probabilistically concat transition operators together using a Bernoulli
--   distribution with the supplied success probability.
--
--   This is just a generalization of sampleT.
bernoulliT
  :: PrimMonad m
  => Double
  -> Transition m a
  -> Transition m a
  -> Transition m a
bernoulliT p t0 t1 = do
  heads <- lift (MWC.bernoulli p)
  if heads then t0 else t1

-- | Probabilistically concat transition operators together via a uniform
--   distribution.
sampleAllT :: PrimMonad m => [Transition m a] -> Transition m a
sampleAllT ts = do
  j <- lift (MWC.uniformR (0, length ts - 1))
  ts !! j

-- | Probabilistically concat transition operators together using the supplied
--   frequency distribution.
--
--   This function is more-or-less an exact copy of 'QuickCheck.frequency',
--   except here applied to transition operators.
frequency :: PrimMonad m => [(Int, Transition m a)] -> Transition m a
frequency xs = lift (MWC.uniformR (1, tot)) >>= (`pick` xs) where
  tot = sum . map fst $ xs
  pick n ((k, v):vs)
    | n <= k = v
    | otherwise = pick (n - k) vs
  pick _ _ = error "frequency: no distribution specified"

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
-- >>> withSystemRandom . asGenIO $ mcmc 3 [0, 0] (metropolis 0.5) rosenbrock
-- -0.48939312153007863,0.13290702689491818
-- 1.4541485365128892e-2,-0.4859905564050404
-- 0.22487398491619448,-0.29769783186855125
mcmc
  :: Show (t a)
  => Int
  -> t a
  -> Transition IO (Chain (t a) b)
  -> Target (t a)
  -> Gen RealWorld
  -> IO ()
mcmc n chainPosition transition chainTarget gen = runEffect $
        chain transition Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ print
  where
    chainScore    = lTarget chainTarget chainPosition
    chainTunables = Nothing

-- A Markov chain driven by an arbitrary transition operator.
chain
  :: PrimMonad m
  => Transition m b
  -> b
  -> Gen (PrimState m)
  -> Producer b m a
chain transition = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT transition state) prng)
    yield next
    loop next prng

