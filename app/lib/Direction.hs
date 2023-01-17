module Direction where

import Linear hiding (normalize)
import Types

normalize :: Direction -> Direction
normalize (V2 0 0) = V2 0 0
normalize (V2 dx 0) = V2 (dx `div` abs dx) 0
normalize (V2 0 dy) = V2 0 (dy `div` abs dy)
normalize (V2 dx dy) = V2 (dx `div` abs dx) (dy `div` abs dy)

getNormalized :: GridPosition -> GridPosition -> Direction
getNormalized (GridPosition (V2 fromX fromY)) (GridPosition (V2 toX toY)) =
  normalize (V2 (toX - fromX) (toY - fromY))
