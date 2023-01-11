module Asteroids.List where

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs [x,y] = [(x, y)]
pairs (x:y:xs) = (x, y) : pairs xs
