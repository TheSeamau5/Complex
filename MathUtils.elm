module MathUtils where

signum x = 
  if | x == 0    ->  0
     | x <  0    -> -1
     | otherwise ->  1