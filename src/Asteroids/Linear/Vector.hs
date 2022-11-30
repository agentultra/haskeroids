module Asteroids.Linear.Vector where

import Linear

clampVector :: (Floating a, Ord a) => V2 a -> a -> V2 a
clampVector (V2 vx vy) vmax =
  let len = sqrt ((vx ** 2) + (vy ** 2))
      multiplier
        | len > vmax = vmax / len
        | otherwise  = 1.0
  in V2 (vx * multiplier) (vy * multiplier)
