module Trainer.Internal where

boxMueller :: Double -> Double -> Double -> Double -> Double
boxMueller μ σ r1 r2 = μ + σ * sqrt (-2*log r1) * cos (2*pi*r2)

positiveStdNormal :: Double -> Double -> Double -> Double
positiveStdNormal hi r1 r2 = min hi (abs bm)
                           where bm = boxMueller 0 (hi/25) r1 r2

