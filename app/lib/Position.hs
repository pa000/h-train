module Position where

import Data.Sequence
import Linear
import Types

onGrid :: Position -> V2 Float
onGrid (Position [] _) = 0.0
onGrid (Position [_] _) = 0.0
onGrid (Position ((start : next : rest)) progress) =
  if startNextDistance < progress
    then onGrid (Position (next : rest) (progress - startNextDistance))
    else
      let dir = Linear.normalize $ nextPos ^-^ startPos
       in startPos ^+^ (dir ^* progress)
  where
    startPos =
      let GridPosition v = start
       in toFloatVector v
    nextPos =
      let GridPosition v = next
       in toFloatVector v
    startNextDistance = Linear.distance startPos nextPos

getRotation :: Position -> Float
getRotation (Position [] _) = 0.0
getRotation (Position [_] _) = 0.0
getRotation (Position (start : next : _) _) =
  let dir = nextPos ^-^ startPos
   in Linear.unangle dir * 180 / pi
  where
    startPos =
      let GridPosition v = start
       in toFloatVector v
    nextPos =
      let GridPosition v = next
       in toFloatVector v

toFloatVector :: (Integral a1, Num a2) => V2 a1 -> V2 a2
toFloatVector = fmap fromIntegral