{-# OPTIONS -Wall #-}

module GridPosition where

import Linear
import Types

distance :: GridPosition -> GridPosition -> Float
distance (GridPosition pos) (GridPosition pos') =
  Linear.distance (fmap fromIntegral pos) (fmap fromIntegral pos')