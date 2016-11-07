{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.MCMC.Anneal (
    anneal
  ) where

import Control.Monad.Trans.State.Strict (get, modify)
import Data.Sampling.Types (Transition, Chain(..), Target(..))

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

