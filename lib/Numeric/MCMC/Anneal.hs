{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.MCMC.Anneal
-- Copyright: (c) 2015 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- Transition operators can easily be tweaked to operate over an /annealed/
-- parameter space, which can be useful when sampling from bumpy landscapes
-- with isolated modes.
--
-- This library exports a single 'anneal' function that allows one to run a
-- /declarative/-compatible transition operator over a space that has been
-- annealed to a specified temperature.
--
-- > import Numeric.MCMC
-- >
-- > annealingTransition = do
-- >   anneal 0.70 (metropolis 1)
-- >   anneal 0.05 (metropolis 1)
-- >   anneal 0.05 (metropolis 1)
-- >   anneal 0.70 (metropolis 1)
-- >   metropolis 1
--
-- These annealed operators can then just be used like any other:
--
-- > himmelblau :: Target [Double]
-- > himmelblau = Target lHimmelblau Nothing where
-- >   lHimmelblau :: [Double] -> Double
-- >   lHimmelblau [x0, x1] =
-- >     (-1) * ((x0 * x0 + x1 - 11) ^ 2 + (x0 + x1 * x1 - 7) ^ 2)
-- >
-- > main :: IO ()
-- > main = withSystemRandom . asGenIO $
-- >   mcmc 10000 [0, 0] annealingTransition himmelblau

module Numeric.MCMC.Anneal (
    anneal
  ) where

import Control.Monad.Trans.State.Strict (get, modify)
import Data.Sampling.Types (Transition, Chain(..), Target(..))

-- | An annealing transformer.
--
--   When executed, the supplied transition operator will execute over the
--   parameter space annealed to the supplied inverse temperature.
--
--   > let annealedTransition = anneal 0.30 (slice 0.5)
anneal
  :: (Monad m, Functor f)
  => Double
  -> Transition m (Chain (f Double) b)
  -> Transition m (Chain (f Double) b)
anneal invTemp baseTransition
  | invTemp < 0 = error "anneal: invalid temperture"
  | otherwise = do
      Chain {..} <- get
      let annealedTarget = annealer invTemp chainTarget
      modify $ useTarget annealedTarget
      baseTransition
      modify $ useTarget chainTarget

annealer :: Functor f => Double -> Target (f Double) -> Target (f Double)
annealer invTemp target = Target annealedL annealedG where
  annealedL xs = invTemp * lTarget target xs
  annealedG    =
    case glTarget target of
      Nothing -> Nothing
      Just g  -> Just (fmap (* invTemp) . g)

useTarget :: Target a -> Chain a b -> Chain a b
useTarget newTarget Chain {..} =
  Chain newTarget (lTarget newTarget chainPosition) chainPosition chainTunables

