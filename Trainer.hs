module Trainer where

import System.Random
import Trainer.Internal

randomListIndex :: Double -> IO Int
randomListIndex hi = do r1 <- randomRIO (0,1)
                        r2 <- randomRIO (0,1)
                        return (floor $ positiveStdNormal hi r1 r2)
