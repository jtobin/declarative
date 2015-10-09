{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC

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

