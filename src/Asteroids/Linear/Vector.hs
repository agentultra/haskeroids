module Asteroids.Linear.Vector where

import Linear

clampVector :: (Floating a, Ord a) => V2 a -> a -> V2 a
clampVector (V2 vx vy) vmax =
  let len = sqrt ((vx ** 2) + (vy ** 2))
      multiplier
        | len > vmax = vmax / len
        | otherwise  = 1.0
  in V2 (vx * multiplier) (vy * multiplier)

length :: Floating a => V2 a -> a
length (V2 x y) = sqrt (x ** 2.0 + y ** 2.0)
